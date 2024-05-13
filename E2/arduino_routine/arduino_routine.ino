/*
 * ARDUINO ROUTINE FOR ILLUSORY TEMPO: MOTOR SPLIT RANGE
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

// Hardware-related variables
SdReader card;    // This object holds the information for the card
FatVolume vol;    // This holds the information for the partition on the card
FatReader root;   // This holds the information for the filesystem on the card
FatReader f;      // This holds the information for the file we'll play
WaveHC wave;      // This is the only wave (audio) object, since we will only play one at a time
int fsrAnalogPin = 0; // FSR is connected to analog 0
int fsrReading;      // the analog reading from the FSR resistor divider

// Constant variables / settings
const int threshold = 1;  // FSR threshold to be considered a tap
const int nsync_tones = 8;  // Number of synchronization tones per trial
const int ncont_tones = 16;  // Number of continuation tones per trial
const int cont_tone_delay_ms = 18;  // Delay between tap and continuation tone start (hardware latency adds extra 2 ms)
const int tone_dur_ms = 250;  // Duration of tones in milliseconds
const int debounce_time = 80;  // Minimum ms allowed between a release and the next tap
const int min_tap_time = 20;  // Minimum ms allowed between a tap and its release

// List of tone file names; names MUST conform to 8.3 standard (<= 8 character name + 3 character extension)
const char *tones[] = {"toneA2.wav", "toneDs3.wav", "toneA3.wav", "toneDs4.wav", "toneA4.wav", "toneDs5.wav",
    "toneA5.wav", "toneDs6.wav", "toneA6.wav", "toneDs7.wav", "toneA7.wav"};

// Condition variables
char MODE = 'I';  // The current mode (e.g., synchronization, continuation, intertrial)
char ioi_chars[3];  // Stores characters that make up the IOI as we read them in (later converted to integer)
int IOI = 500;  // Integer IOI of the current trial
char tone_register;  // Register of the current trial (L or U)
int tone_index = 5;  // Index of the current trial's tone within the tones list (Default is 5, i.e., D#5)

// Timing variables
unsigned long now;  // Most recent time check
unsigned long fsr_time;  // Timing of most recent FSR reading
unsigned long peak_fsr_time;  // Timing of peak FSR pressure
unsigned long next_tone_time;  // Either the timing onset of the next tone, or the earliest time a tone would be allowed
unsigned long next_tap_allowed = 0;  // Earliest time next tap is allowed
unsigned long release_allowed = 0;  // Earliest time next release is allowed

// FSR tapping variables
int FSR = 0;  // Will store the FSR reading
int peak_FSR = 0;  // Will store the peak FSR reading since the last tap release
bool TAPPED = false;  // Flags whether a new tap has been detected; only true during the cycle the tap began
bool RELEASED = true;  // Flags whether the previous tap has ended; true until the next tap begins

// Counters
int tone_num;  // Current tone number on present trial
int sync_tones_played;  // Number of synchronization tones completed on current trial
int cont_tones_played;  // Number of continuation tones completed on current trial
bool tone_loaded = false;
bool tone_queued = false;

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
    loadsound(tones[tone_index]);
    wave.play();
    MODE = 'I';
    Serial.write(MODE);
  }

  // Except during intertrial periods, take FSR reading and notify computer if there was a tap
  if (MODE == 'I') {
    TAPPED = false;
    RELEASED = true;
  } else {
    fsr_time = millis();
    FSR = analogRead(fsrAnalogPin);
    // Keep track of the current tap's peak tap pressure and its timing (peak_FSR is reset every time a new tap happens)   
    if (FSR > peak_FSR) {
      peak_FSR = FSR;
      peak_fsr_time = fsr_time;
    }
    // If this is the first sample to exceed the pressure threshold since the last release's debounce period, mark it as a new tap
    if (RELEASED && (FSR >= threshold) && (fsr_time >= next_tap_allowed)) {
      set_timebuffer(fsr_time);
      Serial.write('T');
      Serial.write(timebuffer, sizeof(timebuffer));
      // The initial peak pressure of a tap is always the first sample of the tap
      peak_FSR = FSR;
      peak_fsr_time = fsr_time;
      // Flag that the person just tapped (for use in triggering continuation tones)
      TAPPED = true;
      RELEASED = false;
      // Set debounce to avoid falsely detecting a release early in the tap
      release_allowed = fsr_time + min_tap_time;
    } else {
      TAPPED = false;
      // If this is the first sample to fall below threshold since the current tap's debounce period ended, mark it as a release
      if (!RELEASED && (FSR < threshold) && (fsr_time >= release_allowed)) {
        set_timebuffer(fsr_time);
        Serial.write('R');
        Serial.write(timebuffer, sizeof(timebuffer));
        set_timebuffer(peak_fsr_time);
        Serial.write(peak_FSR);
        Serial.write(timebuffer, sizeof(timebuffer));
        // Flag that the person released their tap and set debounce to avoid falsely detecting a new tap as their finger lifts from the FSR
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
  // Reads the register, pitch level, and interonset interval for the current trial and sets up a few counters
  // The trial condition is sent to the Arduino as a sequence of five characters. The first is the register
  // (L = Lower, U = Upper), the second is the pitch level (0 to 5, corresponding to order within a register),
  // and the final three characters indicate the interonset interval in ms. Note this assumes three-digit IOIs.
  if (MODE == 'B') {
    while (Serial.available() < 5) {}  // Wait until we have all condition characters ready to read

    // Read the register and pitch level within that register, then convert to an index on the list of tone names
    tone_register = Serial.read();
    tone_index = Serial.read() - 48;
    if (tone_register == 'U') {
      tone_index = tone_index + 5;
    }

    // Read the interonset interval
    for (int i = 0; i < 3; i++) {
      ioi_chars[i] = Serial.read();
    }
    IOI = atoi(ioi_chars);

    // Preload the tone to be used on the trial
    loadsound(tones[tone_index]);
    tone_loaded = true;

    // Set up counters, then begin trial
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
    
    if (!wave.isplaying) {
      // Load next tone as soon as a tone has finished playing, so FSR blindspot during
      // file loading occurs at a time the participant is unlikely to be tapping
      if (!tone_loaded) {
        loadsound(tones[tone_index]);
        tone_loaded = true;
      }
      now = millis();
      if (now >= next_tone_time) {
        wave.play();
        set_timebuffer(millis());
        Serial.write(MODE);
        Serial.write(timebuffer, sizeof(timebuffer));
        tone_loaded = false;
        sync_tones_played++;
        if (sync_tones_played == nsync_tones) {
          MODE = 'C';
          tone_num = 0;
        } else {
          next_tone_time = now + IOI;
      }
    }

    }
  }

  // CONTINUATION TAPPING
  // Disallow further action until the previous tone has finished
  // End trial when final continuation tone ends
  // If a tone has been queued by a tap to occur now, play it
  // If no tone is currently queued and the person taps, queue the next tone
  // to play after a set delay (e.g., 20 ms)
  if (MODE == 'C') {

    // If we haven't cued the next tone, and the most recent tone has finished playing...
    if (!tone_queued) {
      if (!wave.isplaying) {
        // Go back to idling if the final tone of the trial just finished
        if (cont_tones_played == ncont_tones) {
          MODE = 'I';
          Serial.write(MODE);
          tone_queued = false;
        // Otherwise, make sure the next tone is loaded, and queue it when the person taps
        } else {
          if (!tone_loaded) {
            loadsound(tones[tone_index]);
            tone_loaded = true;
          }
          if (TAPPED) {
            next_tone_time = fsr_time + cont_tone_delay_ms;
            tone_queued = true;
          }
        }
      }

    // If we have cued the next tone, play it when it's the right time      
    } else if (fsr_time >= next_tone_time) {
      wave.play();
      set_timebuffer(millis());
      Serial.write(MODE);
      Serial.write(timebuffer, sizeof(timebuffer));
      tone_loaded = false;
      tone_queued = false;
      cont_tones_played++;
    }
  }
}

void loadsound(char *name) {
  // Look in the root directory and open the specified audio file
  if (!f.open(root, name)) {
    putstring("Couldn't open file "); Serial.println(name); return;
  }
  // Read the file and create a new wave object from it
  if (!wave.create(f)) {
    putstring_nl("Not a valid WAV"); return;
  }
}

void set_timebuffer(unsigned long time) {
  timebuffer[0] = time & 255;
  timebuffer[1] = (time >> 8)  & 255;
  timebuffer[2] = (time >> 16) & 255;
  timebuffer[3] = (time >> 24) & 255;
}
