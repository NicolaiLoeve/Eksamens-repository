#include <HCSR04.h>

const byte triggerPin = 3;
const byte echoPin    = 2;

// Hvornår noget regnes som "foran sensoren" (justér efter opsætning)
const int DIST_THRESHOLD_CM = 200;

// Hvor ofte vi måler (ms)
const unsigned long SAMPLE_INTERVAL_MS = 50;

// Hvor mange sammenhængende "tæt"-målinger der kræves for at starte en person
const int CONSECUTIVE_CLOSE_NEEDED = 2;

// Hvor mange sammenhængende "langt væk"-målinger der kræves for at afslutte en person
const int CONSECUTIVE_FAR_NEEDED   = 2;

// Logik-variabler
long peopleCount = 0;
bool trackingPerson = false;        // følger vi lige nu en person?
double currentMinDistance = 0;      // mindste afstand for den person vi følger
unsigned long lastSampleTime = 0;

// tællere til at undgå at støj starter/stopper personer
int closeStreak = 0;
int farStreak   = 0;

void setup() {
  Serial.begin(9600);
  HCSR04.begin(triggerPin, echoPin);
}

void loop() {
  unsigned long now = millis();

  if (now - lastSampleTime < SAMPLE_INTERVAL_MS) {
    return;
  }
  lastSampleTime = now;

  // Mål afstand
  double* distances = HCSR04.measureDistanceCm();
  double distance = distances[0];   // cm

  // vi ignorerer åbenlyst forkerte målinger (0 eller negativt)
  if (distance <= 0) {
    return;
  }

  bool close = (distance < DIST_THRESHOLD_CM);

  // ---------- HVIS VI IKKE FØLGER NOGEN LIGE NU ----------
  if (!trackingPerson) {
    if (close) {
      closeStreak++;
      if (closeStreak >= CONSECUTIVE_CLOSE_NEEDED) {
        // start ny person
        peopleCount++;
        trackingPerson = true;
        currentMinDistance = distance;
        farStreak = 0;
      }
    } else {
      closeStreak = 0;
    }
  }

  // ---------- HVIS VI ER I GANG MED AT FØLGE EN PERSON ----------
  else {
    if (close) {
      // personen er stadig i feltet -> opdatér minimums-afstand
      if (distance < currentMinDistance) {
        currentMinDistance = distance;
      }
      farStreak = 0;
    } else {
      // personen er på vej ud af feltet
      farStreak++;
      if (farStreak >= CONSECUTIVE_FAR_NEEDED) {
        // PERSONEN ER VÆK -> PRINT ÉN LINJE
        // Format: personnummer, mindste afstand i cm
        Serial.print(peopleCount);
        Serial.print(",");
        Serial.println(currentMinDistance, 0);  // 0 decimaler

        // nulstil state
        trackingPerson = false;
        closeStreak = 0;
        farStreak = 0;
      }
    }
  }
}