#|;; Add to your main file (after globals)
(ql:quickload :cl-midi :silent t)

(defun rune-to-midi-hook (rune &optional (bpm 120) (duration 0.25))
  "Generates a simple MIDI file from a rune sequence + current vibe."
  (let* ((vibe (or (and (fboundp '%safe-current-cyclic-group) (%safe-current-cyclic-group)) "DEFAULT"))
         (base-notes '(60 62 64 65 67 69 71 72))  ; C major scale
         (vibe-offset (cond ((string= vibe "GRIME") 12)     ; lower octave
                            ((string= vibe "DRIP") 0)
                            ((string= vibe "UKG") 24)       ; higher
                            (t 12)))
         (midi-file (format nil "peteai-rune-hook-~a.mid" (now-ms)))
         (midi (cl-midi:make-midi-file :format 1 :division 480)))

    (cl-midi:add-track midi 0)
    (cl-midi:set-tempo midi 0 (* bpm 60 1000000))  ; microseconds per quarter note

    (loop for sym in rune
          for i from 0
          for note = (+ (nth (mod i (length base-notes)) base-notes) vibe-offset)
          do (cl-midi:note-on midi 0 note 0 100)   ; channel 0, velocity 100
             (cl-midi:note-off midi 0 note duration 100))

    (cl-midi:write-midi-file midi-file midi)
    (format t "~&Rune MIDI hook generated: ~a (Vibe: ~a, BPM: ~a)~%" midi-file vibe bpm)
    
    ;; Open in player (cross-platform)
    (uiop:run-program (list (cond ((uiop:os-windows-p) "start")
                                  ((uiop:os-macosx-p) "open")
                                  (t "xdg-open"))
                          midi-file)
                      :ignore-error-status t)
    midi-file))|#
    
    
;;;; ------------------------------------------------------------
;;;; Rune -> MIDI Hook (Quicklisp :midi / midi-20070618)
;;;; ------------------------------------------------------------

(ql:quickload :midi)

(defun now-ms ()
  "Good-enough timestamp for filenames."
  (round (* 1000 (/ (get-internal-real-time)
                    internal-time-units-per-second))))

(defun midi-msg (class time &rest initargs)
  "Make a MIDI message instance, set absolute TIME (ticks)."
  (let ((m (apply #'make-instance class initargs)))
    (setf (midi:message-time m) time)
    m))

(defun %set-status! (msg status)
  "Set internal status byte used by midi-20070618 writer."
  (or (ignore-errors (setf (slot-value msg 'midi::status) status) t)
      (ignore-errors (setf (slot-value msg 'midi::message-status) status) t)
      (error "Can't set status slot; (describe msg) and adjust slot name.")))

(defun set-channel! (msg channel)
  "Encode CHANNEL (0..15) into the status byte."
  (let ((ch (max 0 (min 15 channel))))
    (cond
      ((typep msg 'midi:note-on-message)  (%set-status! msg (+ #x90 ch)))
      ((typep msg 'midi:note-off-message) (%set-status! msg (+ #x80 ch)))))
  msg)

(defun note-on (time key vel &key (channel 0))
  (set-channel!
   (midi-msg 'midi:note-on-message time :key key :velocity vel)
   channel))

(defun note-off (time key &key (channel 0) (vel 0))
  (set-channel!
   (midi-msg 'midi:note-off-message time :key key :velocity vel)
   channel))

(defun bpm->usec-per-quarter (bpm)
  (round (/ 60000000 (max 1 bpm))))

(defun rune-to-midi-hook (rune &key (bpm 120) (duration 0.25) (division 480) (channel 0))
  "Generates a simple MIDI file from a RUNE sequence + current vibe pack.

RUNE: list of symbols/strings (we just step through them)
BPM: tempo
DURATION: note length in quarter-notes (0.25 = 16th note)
DIVISION: ticks per quarter note
CHANNEL: MIDI channel 0..15"
  (let* ((vibe (or (and (fboundp '%safe-current-cyclic-group) (%safe-current-cyclic-group))
                   "DEFAULT"))
         (base-notes '(60 62 64 65 67 69 71 72)) ; C major
         (vibe-offset (cond ((string= vibe "GRIME") 0)     ; keep mid/low
                            ((string= vibe "DRIP") 12)
                            ((string= vibe "UKG") 24)
                            ((string= vibe "TRILL") 12)
                            ((string= vibe "DUBSTEP") 0)
                            (t 12)))
         (file (format nil "peteai-rune-hook-~a.mid" (now-ms)))
         (q division)
         ;; convert duration in quarter-notes -> ticks
         (dur-ticks (round (* q duration)))
         (usec (bpm->usec-per-quarter bpm))
         (time 0)
         (events
           (list
            ;; tempo meta-message at time 0
            (midi-msg 'midi:tempo-message 0 :tempo usec))))

    ;; build note events (absolute time)
    (dolist (sym rune)
      (declare (ignore sym))
      (let* ((i (mod (floor (/ time (max 1 dur-ticks))) (length base-notes)))
             (note (+ (nth i base-notes) vibe-offset)))
        (push (note-on  time note 100 :channel channel) events)
        (push (note-off (+ time dur-ticks) note :channel channel :vel 0) events)
        (incf time dur-ticks)))

    ;; writer expects track as a list of timed messages (in any order is OK,
    ;; but it's nicer to sort by time)
    (setf events (sort events #'< :key #'midi:message-time))

    (midi:write-midi-file
     (make-instance 'midi:midifile
                    :format 1
                    :division division
                    :tracks (list events))
     file)

    (format t "~&Rune MIDI hook generated: ~a (Vibe: ~a, BPM: ~a, dur: ~a qn)~%"
            file vibe bpm duration)

    ;; open it (Linux)
    (when (find-package :uiop)
      (ignore-errors
        (uiop:run-program (list "xdg-open" file) :ignore-error-status t)))

    file))

