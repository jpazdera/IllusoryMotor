/*
 * Initial setup:
 * 1) Install WaveHC library.
 * 2) Open WaveHC.h and change DVOLUME setting from 0 to 1.
 * 3) Upload arduino_routine.ino to your Arduino.
 * 
 * Communication with the experiment occurs via serial connection. The communication codes
 * are as follows:
 * H = Sent by the experiment to test the serial connection. ("Hello")
 * P = Sent by the experiment to indicate the start of the SPR task. ("Production")
 * B = Sent by the experiment to indicate that a trial should begin. ("Begin")
 * S = Sent by the Arduino to indicate a sync tap has started. ("Synchronization")
 * C = Sent by the Arduino to indicate a continuation tap has started. ("Continuation")
 * I = Sent by the Arduino to indicate that a trial has ended. ("Intertrial")
 * T = Sent by the Arduino to indicate that a tap has begun. ("Tap")
 * R = Sent by the Arduino to indicate that a tap has ended. ("Release")
 * 
 * Things to note:
 * The code as written assumes the trial condition specifier is exactly 4 characters (1 for octave, 3 for IOI).
*/

#include <FatReader.h>
#include <SdReader.h>
#include <avr/pgmspace.h>
#include "WaveUtil.h"
#include "WaveHC.h"

SdReader card;    // This object holds the information for the card
FatVolume vol;    // This holds the information for the partition on the card
FatReader root;   // This holds the information for the filesystem on the card
FatReader f;      // This holds the information for the file we'll play

WaveHC wave;      // This is the only wave (audio) object, since we will only play one at a time
int fsrAnalogPin = 0; // FSR is connected to analog 0
int fsrReading;      // the analog reading from the FSR resistor divider

const int threshold = 1;  // FSR threshold to be considered a tap
const int nsync_tones = 8;  // Number of synchronization tones per trial
const int ncont_tones = 16;  // Number of continuation tones per trial
const int cont_tone_delay_ms = 18;  // Delay between tap and continuation tone start (hardware latency adds extra 2 ms)
const int tone_dur_ms = 250;  // Duration of tones in milliseconds
const int debounce_time = 80;  // Minimum ms allowed between a release and the next tap
const int min_tap_time = 20;  // Minimum ms allowed between a tap and its release

char MODE = 'I';  // Will store the current mode (synchronization, continuation, intertrial)
int FSR;  // Will store the FSR reading
char ioi_chars[] = "500";  // Will store the characters that make up the IOI as we read them in
int IOI = 500;  // Will store the IOI of the current trial
bool TAPPED = false;  // Flags whether a new tap has been detected
bool RELEASED = true;  // Flags whether the previous tap has ended
char TONE[] = "toneA4.wav";
unsigned long now;  // Used for storing times
unsigned long fsr_time;
int tone_num;
int sync_tones_played;
int cont_tones_played;
unsigned long next_tone_time;
unsigned long next_tap_allowed = 0;
unsigned long release_allowed = 0;
byte timebuffer[4];  // 4-byte buffer that can be used to send timestamp longs over serial

// this handy function will return the number of bytes currently free in RAM, great for debugging!   
int freeRam(void) {
  extern int  __bss_end; 
  extern int  *__brkval; 
  int free_memory; 
  if((int)__brkval == 0) {
    free_memory = ((int)&free_memory) - ((int)&__bss_end); 
  }
  else {
    free_memory = ((int)&free_memory) - ((int)__brkval); 
  }
  return free_memory; 
} 

void sdErrorCheck(void)
{
  if (!card.errorCode()) return;
  putstring("\n\rSD I/O error: ");
  Serial.print(card.errorCode(), HEX);
  putstring(", ");
  Serial.println(card.errorData(), HEX);
  while(1);
}

void setup() {
  Serial.begin(9600);

  // set up serial port
  Serial.begin(9600);
  putstring_nl("WaveHC with 6 buttons");
  
   putstring("Free RAM: ");       // This can help with debugging, running out of RAM is bad
  Serial.println(freeRam());      // if this is under 150 bytes it may spell trouble!
  
  // Set the output pins for the DAC control. This pins are defined in the library
  pinMode(2, OUTPUT);
  pinMode(3, OUTPUT);
  pinMode(4, OUTPUT);
  pinMode(5, OUTPUT);
 
  // pin13 LED
  pinMode(13, OUTPUT);
 
  //  if (!card.init(true)) { //play with 4 MHz spi if 8MHz isn't working for you
  if (!card.init()) {         //play with 8 MHz spi (default faster!)  
    putstring_nl("Card init. failed!");  // Something went wrong, lets print out why
    sdErrorCheck();
    while(1);                            // then 'halt' - do nothing!
  }
  
  // enable optimize read - some cards may timeout. Disable if you're having problems
  card.partialBlockRead(true);
 
// Now we will look for a FAT partition!
  uint8_t part;
  for (part = 0; part < 5; part++) {     // we have up to 5 slots to look in
    if (vol.init(card, part)) 
      break;                             // we found one, lets bail
  }
  if (part == 5) {                       // if we ended up not finding one  :(
    putstring_nl("No valid FAT partition!");
    sdErrorCheck();      // Something went wrong, lets print out why
    while(1);                            // then 'halt' - do nothing!
  }
  
  // Lets tell the user about what we found
  putstring("Using partition ");
  Serial.print(part, DEC);
  putstring(", type is FAT");
  Serial.println(vol.fatType(),DEC);     // FAT16 or FAT32?
  
  // Try to open the root directory
  if (!root.openRoot(vol)) {
    putstring_nl("Can't open root dir!"); // Something went wrong,
    while(1);                             // then 'halt' - do nothing!
  }
  
  // Whew! We got past the tough parts.
  putstring_nl("Ready!");
}

void loop() {
  
  // Get mode-change readings from Python via serial connection
  if (Serial.available() > 0) {
    MODE = Serial.read();
  }

  // Computer will test connection by sending an "H" for "Hi"; respond with a tone and return message
  if (MODE == 'H') {
    playfile(TONE);
    MODE = 'I';
    Serial.write(MODE);
  }

  // Except during intertrial periods, take FSR reading and notify computer if there was a tap
  // Suppress releases for a set number of time after a tap begins (to avoid false releases)
  // Suppress additional tap triggers until a set number of ms after the person lifts their finger (to avoid false detection)
  if (MODE == 'I') {
    TAPPED = false;
    RELEASED = true;
  } else {
    fsr_time = millis();
    FSR = analogRead(fsrAnalogPin);
    if (RELEASED && (FSR >= threshold) && (fsr_time >= next_tap_allowed)) {
      set_timebuffer(fsr_time);
      Serial.write('T');
      Serial.write(timebuffer, sizeof(timebuffer));
      TAPPED = true;
      RELEASED = false;
      release_allowed = fsr_time + min_tap_time;
    } else {
      TAPPED = false;
      if (!RELEASED && (FSR < threshold) && (fsr_time >= release_allowed)) {
        set_timebuffer(fsr_time);
        Serial.write('R');
        Serial.write(timebuffer, sizeof(timebuffer));
        RELEASED = true;
        next_tap_allowed = fsr_time + debounce_time;
      }
    }
  }

  // SPR Task
  // Measure the spontaneous production rate (SPR) of the participant by recording taps without feedback
  // Since the Arduino only needs to loop its FSR reading, we don't actually need a mode-specific routine
  // if (MODE == "P") {}

  // BEGIN TRIAL
  // Reads the octave and interonset interval for the current trial and sets up a few counters
  if (MODE == 'B') {
    while (Serial.available() < 4) {}  // Wait until we have all four condition characters to read
    TONE[5] = Serial.read();
    for (int i = 0; i < 3; i++) {
      ioi_chars[i] = Serial.read();
    }
    IOI = atoi(ioi_chars);
    sync_tones_played = 0;
    cont_tones_played = 0;
    next_tone_time = 0;
    MODE = 'S';
  }

  // SYNCHRONIZATION TAPPING
  // Trigger next tone if IOI has elapsed since previous tone
  // Activate continuation tapping phase if final sync tone has started
  // Otherwise, set the time of the next sync tone
  if (MODE == 'S') {
    now = millis();
    if (now >= next_tone_time) {    
      playfile(TONE);
      set_timebuffer(millis());
      Serial.write(MODE);
      Serial.write(timebuffer, sizeof(timebuffer));
      sync_tones_played++;
      if (sync_tones_played == nsync_tones) {
        MODE = 'C';
        tone_num = 0;
        next_tone_time = now + tone_dur_ms;
      } else {
        next_tone_time = now + IOI;
      }
    }
  }

  // CONTINUATION TAPPING
  // Disallow further action until the previous tone has finished
  // End trial when final continuation tone ends
  // If a tap has been detected, play the tone after a set delay (e.g., 20 ms)
  if (MODE == 'C') {
    if (fsr_time >= next_tone_time) {
      if (cont_tones_played == ncont_tones) {
        MODE = 'I';
        Serial.write(MODE);
      } else if (TAPPED) {
        delay(fsr_time + cont_tone_delay_ms - millis());
        playfile(TONE);
        set_timebuffer(millis());
        Serial.write(MODE);
        Serial.write(timebuffer, sizeof(timebuffer));
        next_tone_time = fsr_time + cont_tone_delay_ms + tone_dur_ms;
        cont_tones_played++;
      }
    }
  }

}

void playfile(char *name) {
  // see if the wave object is currently doing something
  if (wave.isplaying) {// already playing something, so stop it!
    wave.stop(); // stop it
  }
  // look in the root directory and open the file
  if (!f.open(root, name)) {
    putstring("Couldn't open file "); Serial.println(name); return;
  }
  // OK read the file and turn it into a wave object
  if (!wave.create(f)) {
    putstring_nl("Not a valid WAV"); return;
  }
  
  // ok time to play! start playback
  wave.play();
}

void set_timebuffer(unsigned long time) {
  timebuffer[0] = time & 255;
  timebuffer[1] = (time >> 8)  & 255;
  timebuffer[2] = (time >> 16) & 255;
  timebuffer[3] = (time >> 24) & 255;
}
