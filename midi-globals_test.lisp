(ql:quickload :midi)

(defun bpm->usec-per-quarter (bpm)
  (round (/ 60000000 (max 1 bpm))))

(defun %set-status! (msg status)
  (or (ignore-errors (setf (slot-value msg 'midi::status) status) t)
      (ignore-errors (setf (slot-value msg 'midi::message-status) status) t)
      (error "Can't set status slot on ~S. Run (describe msg) to see slots." msg)))

(defun set-channel! (msg channel)
  (let ((ch (max 0 (min 15 channel))))
    (cond
      ((typep msg 'midi:note-on-message)  (%set-status! msg (+ #x90 ch)))
      ((typep msg 'midi:note-off-message) (%set-status! msg (+ #x80 ch)))))
  msg)

(defun midi-msg (class time &rest initargs)
  (let ((m (apply #'make-instance class initargs)))
    (setf (midi:message-time m) time)
    m))

(defun note-on (time key vel &key (channel 0))
  (set-channel!
   (midi-msg 'midi:note-on-message time :key key :velocity vel)
   channel))

(defun note-off (time key &key (channel 0) (vel 0))
  (set-channel!
   (midi-msg 'midi:note-off-message time :key key :velocity vel)
   channel))
   
   
;; Helpers: note-on / note-off using raw status + data (channel messages)
(defun note-on (time key vel &key (channel 0))
  (make-instance 'midi:message
                 :time time
                 :status (+ #x90 (mod channel 16))
                 :data (coerce (list key vel) 'vector)))

(defun note-off (time key &key (channel 0) (vel 0))
  (make-instance 'midi:message
                 :time time
                 :status (+ #x80 (mod channel 16))
                 :data (coerce (list key vel) 'vector)))
                 
;; Helper: note-on / note-off (channel messages — status 90/80 + ch)
(defun note-on (time key vel &key (channel 0))
  (make-instance 'midi:message
                 :time time
                 :status (+ #x90 (mod channel 16))
                 :data (coerce (list key vel) 'vector)))

(defun note-off (time key &key (channel 0) (vel 0))
  (make-instance 'midi:message
                 :time time
                 :status (+ #x80 (mod channel 16))
                 :data (coerce (list key vel) 'vector)))

;; Safe MIDI test — uses only known patterns from your describe
#|(defun midi-test (&optional (filename "peteai-midi-test-120bpm.mid"))
  "120 BPM C major scale — raw meta, known slots"
  (let* ((mid (make-instance 'midi:midifile :format 1 :division 480))
         (track nil))  ; start empty

    ;; Tempo meta (works — your describe confirmed)
    (push (make-instance 'midi:tempo-message :time 0 :tempo 500000) track)

    ;; Time sig 4/4 raw meta bytes (FF 58 04 04 02 18 08)
    (push (make-instance 'midi:message
                         :time 0
                         :status #xFF  ; meta status
                         :data (coerce '(#x58  ; type: time sig
                                         #x04  ; length 4
                                         #x04  ; numerator 4
                                         #x02  ; denom 2 (power-of-2 → 4)
                                         #x18  ; 24 clocks
                                         #x08) ; 8 32nds/quarter
                               'vector))
          track)

    ;; C major scale up/down
    (let ((notes-up '(60 62 64 65 67 69 71 72))
          (tick 0)
          (dur-ticks 480))
      (dolist (note (append notes-up (reverse notes-up)))
        (push (note-on tick note 100 :channel 0) track)
        (push (note-off (+ tick dur-ticks) note :channel 0 :vel 0) track)
        (incf tick dur-ticks)))

    ;; Sort by time
    (setf track (sort track #'< :key #'midi:message-time))

    ;; Attach track — fallback to slot-value since accessor may be internal
    (setf (slot-value mid 'midi::tracks) (list track))

    ;; Write!
    (midi:write-midi-file mid filename)

    (format t "~&🎹 MIDI TEST **LOCKED IN** → ~a~%" filename)
    (format t "   → 120 BPM C major scale up/down~%")
    (format t "   → Play in VLC/GarageBand/LMMS/Audacity~%")
    (ignore-errors (uiop:run-program (list "xdg-open" filename) :ignore-error-status t))
    filename))|#

;; The winning MIDI test — uses public midifile-tracks reader!
#|(defun midi-test (&optional (filename "peteai-midi-test-120bpm.mid"))
  "120 BPM C major scale — proven working with your probes"
  (let* ((mid (make-instance 'midi:midifile :format 1 :division 480))
         (track nil))  ; start empty list

    ;; Tempo (confirmed working)
    (push (make-instance 'midi:tempo-message :time 0 :tempo 500000) track)  ; 120 BPM

    ;; Time sig 4/4 as raw meta message (FF 58 04 04 02 18 08)
    (push (make-instance 'midi:message
                         :time 0
                         :status #xFF
                         :data (coerce '(#x58 #x04 #x04 #x02 #x18 #x08) 'vector))
          track)

    ;; C major scale up & down
    (let ((notes-up '(60 62 64 65 67 69 71 72))
          (tick 0)
          (dur-ticks 480))
      (dolist (note (append notes-up (reverse notes-up)))
        (push (note-on tick note 100 :channel 0) track)
        (push (note-off (+ tick dur-ticks) note :channel 0 :vel 0) track)
        (incf tick dur-ticks)))

    ;; Sort events by time
    (setf track (sort track #'< :key #'midi:message-time))

    ;; Attach track using the **public exported accessor**
    (setf (midi:midifile-tracks mid) (list track))

    ;; Write the file
    (midi:write-midi-file mid filename)

    ;; Victory message
    (format t "~&🎹 **MIDI TEST RUNNING & PLAYABLE** → ~a~%" filename)
    (format t "   → 120 BPM C major scale (up & down)~%")
    (format t "   → 4/4 time sig via raw bytes~%")
    (format t "   → Open in VLC, GarageBand, LMMS, Audacity — listen for the scale!~%")
    (format t "   → If no sound: check volume or try another player~%")
    (ignore-errors (uiop:run-program (list "xdg-open" filename) :ignore-error-status t))
    filename))|#
      

(defun midi-test (&optional (filename "peteai-midi-test-120bpm.mid"))
  "Writes a 120 BPM C major scale up/down using midi-20070618."
  (let* ((division 480)
         (bpm 120)
         (usec (bpm->usec-per-quarter bpm))
         (mid (make-instance 'midi:midifile :format 1 :division division :tracks nil))
         (track '())
         (notes '(60 62 64 65 67 69 71 72 72 71 69 67 65 64 62 60))
         (tick 0)
         (dur division)) ; quarter note

    ;; tempo at time 0
    (push (midi-msg 'midi:tempo-message 0 :tempo usec) track)

    ;; scale notes
    (dolist (n notes)
      (push (note-on tick n 100 :channel 0) track)
      (push (note-off (+ tick dur) n :channel 0 :vel 0) track)
      (incf tick dur))

    (setf track (sort track #'< :key #'midi:message-time))

    ;; midi-20070618 has no (setf midifile-tracks), so set the slot directly
    (setf (slot-value mid 'midi::tracks) (list track))

    (midi:write-midi-file mid filename)
    (format t "~&🎹 Wrote MIDI: ~a~%" filename)
    (ignore-errors (uiop:run-program (list "xdg-open" filename) :ignore-error-status t))
    filename))
    
(defun %set-status! (msg status)
  ;; Cover exported + internal slot names across midi-20070618 builds.
  (or (ignore-errors (setf (slot-value msg 'midi:status) status) t)
      (ignore-errors (setf (slot-value msg 'midi::status) status) t)
      (ignore-errors (setf (slot-value msg 'midi::message-status) status) t)
      (error "Can't set a STATUS slot on ~S. Run (describe msg) to see slots." msg)))

(defun ensure-status! (msg &key (channel 0))
  "Make sure STATUS is bound, for both meta and channel messages."
  (cond
    ;; Tempo/meta messages should be FF
    ((typep msg 'midi:tempo-message)
     (%set-status! msg #xFF))

    ;; Note on/off: status is 0x90/0x80 + channel
    ((typep msg 'midi:note-on-message)
     (%set-status! msg (+ #x90 (max 0 (min 15 channel)))))

    ((typep msg 'midi:note-off-message)
     (%set-status! msg (+ #x80 (max 0 (min 15 channel)))))

    (t msg))
  msg)

(defun midi-msg (class time &rest initargs)
  (let ((m (apply #'make-instance class initargs)))
    (setf (midi:message-time m) time)
    ;; bind status for tempo/meta, and leave channel binding to note-on/off helpers
    (ensure-status! m)
    m))

(defun set-channel! (msg channel)
  (ensure-status! msg :channel channel))


