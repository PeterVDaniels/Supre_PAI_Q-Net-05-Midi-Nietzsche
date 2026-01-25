(defpackage :peteai.nietzsche.trial
  (:use :cl)
  ;;(peteai.nietzsche:nietzsche-deliberate ...)
  (:export :run-nietzsche-trial :play-nietzsche-round))

(in-package :peteai.nietzsche.trial)

(defstruct player-state
  (round 0)
  (history '())
  (notes '()))

(defun init-player-state ()
  "Initializer (do NOT name this make-player-state)."
  (make-player-state :round 0 :history '() :notes '()))

(defparameter *nietzsche-modes*
  '((:genealogy
     "Why does this belief feel necessary to you?"
     "What problem disappears if this belief is true?"
     "When did you inherit or adopt this idea?")
    (:pressure
     "What fear does this belief protect you from?"
     "What happens if you must live without this idea?"
     "Who gains power if this belief is unquestioned?")
    (:reframe
     "Rewrite this without moral language."
     "State it as a goal with tradeoffs instead of a rule."
     "Describe it as a preference, not a command.")
    (:inversion
     "What if the opposite were life-enhancing?"
     "Who might flourish under a reversed value?"
     "What strength is currently being excluded?")
    (:creation
     "What could be built instead of criticized?"
     "What does this belief make possible?"
     "If this were art, what kind would it be?")))
     
(defun %trim (s)
  (string-trim '(#\Space #\Tab #\Newline #\Return) s))

(defun %probe-after-response (input response flags)
  "Generate a short follow-up challenge. No scoring; just precision training."
  (let* ((r (string-downcase (%trim response)))
         (has-universal (or (search "everyone" r) (search "all " r) (search "humanity" r)))
         (has-power (search "power" r)))
    (cond
      ((and has-universal has-power)
       (list
        "When you say 'power', do you mean agency, safety, status, or moral authority?"
        "Name one group/person who might *lose* something under this rule (time, freedom, excellence, risk)."
        "Rewrite the claim as a goal + tradeoff: 'If we want X, we accept Y cost.'"))
      ((and has-universal (member :slave-morality flags))
       (list
        "Universal claims often hide tradeoffs. What tradeoff is being denied here?"
        "Who is the 'protector' in your sentence, and what burden does that place on them?"))
      ((member :slave-morality flags)
       (list
        "Try rewriting without 'must': what outcome are you aiming for, and why?"
        "What concrete policy/action would 'protect equally' require?"))
      (t
       (list
        "What desire does this statement express?"
        "What fear does it reduce?"
        "What would you build from it?")))))


(defun pick-prompts (n)
  (loop repeat n
        collect
        (destructuring-bind (mode . prompts)
            (nth (random (length *nietzsche-modes*)) *nietzsche-modes*)
          (declare (ignore mode))
          (nth (random (length prompts)) prompts))))
          
(defun propose-rewrite (input flags)
  "Return a treaty-style rewrite suggestion that avoids moral-whip framing.
Keeps intent, clarifies tradeoffs."
  (cond
    ((member :slave-morality flags)
     (format nil
             "Goal+Tradeoff rewrite: If we want protection for all, we choose the minimum rules that reduce harm while preserving excellence and freedom, and we state the costs explicitly."))
    ((member :herd-appeal flags)
     (format nil
             "Rewrite: Remove consensus language; state your reasons so the claim stands even if nobody agrees. Original: ~a" input))
    ((member :resentment flags)
     (format nil
             "Rewrite: Replace accusation with construction—name what you would build instead of what you oppose. Original: ~a" input))
    ((member :life-denial flags)
     (format nil
             "Rewrite: Preserve vitality—rewrite the constraint as moderation, not negation. Original: ~a" input))
    (t
     (format nil "Rewrite: ~a" input))))
     
(defun %strip-prefix (s prefix)
  (let ((pos (search prefix s :test #'char-equal)))
    (if (and pos (= pos 0))
        (string-trim '(#\Space #\Tab #\Newline #\Return)
                     (subseq s (length prefix)))
        s)))

;; inside play-nietzsche-round let*:
;; (treaty-candidate (%strip-prefix rewrite "Goal+Tradeoff rewrite:"))

;; and in the returned plist:
;; :treaty-candidate treaty-candidate



          
(defun prompt-move ()
  (format t "~%Choose a move:~%")
  (format t "  [A] Rewrite without moral language~%")
  (format t "  [B] Rewrite as goal + tradeoff~%")
  (format t "  [C] Invert value and defend~%")
  (format t "  [D] Keep claim, clarify definitions~%")
  (format t "Selection: ")
  (string-upcase (string-trim '(#\Space #\Tab) (read-line))))
         

(defun play-nietzsche-round (input &key (prompt-count 3))
  "Run a single Nietzsche trial round and return a PeteAI-friendly plist."
  (let* ((analysis (peteai.nietzsche:nietzsche-deliberate input :return :plist))
         (flags (getf analysis :flags))
         (questions (getf analysis :questions))
         (notes (getf analysis :notes))
         (prompts (pick-prompts prompt-count))
         (rewrite (propose-rewrite input flags))
         (treaty-candidate (%strip-prefix rewrite "Goal+Tradeoff rewrite:")))

    (format t "~%NIETZSCHE TRIAL INITIATED~%")
    (format t "~%Original Statement:~%~a~%" input)

    (when flags
      (format t "~%Detected Pressure Points:~%")
      (dolist (f flags) (format t "  - ~a~%" f)))

    (when notes
      (format t "~%Notes:~%")
      (dolist (n notes) (format t "  * ~a~%" n)))

    (format t "~%Genealogical Questions:~%")
    (dolist (q questions) (format t "  • ~a~%" q))

    (format t "~%Trial Prompts:~%")
    (loop for p in prompts
          for i from 1
          do (format t "  [~a] ~a~%" i p))

    (format t "~%Suggested Treaty Rewrite:~%~a~%" rewrite)

    ;; Interactivity move + response
    (let ((move (prompt-move)))
      (format t "~%✎ Your response (move ~a):~%" move)
      (let* ((response (read-line))
             (followups (%probe-after-response input response flags)))

        (format t "~%Nietzsche follow-up probes:~%")
        (dolist (p followups) (format t "  → ~a~%" p))

        (format t "~%Second response (optional, press Enter to skip):~%")
        (let ((response2 (read-line)))
          (list
           :module :nietzsche-trial
           :input input

           ;; Core analysis
           :analysis analysis
           :flags flags
           :questions questions
           :notes notes

           ;; Trial mechanics
           :prompts prompts
           :move move
           :response response
           :followups followups
           :response2 response2

           ;; Treaty integration
           :rewrite rewrite
           :treaty-candidate treaty-candidate))))))



(defun run-nietzsche-trial (&key (rounds 3))
  "Multi-round REPL loop."
  (let ((state (init-player-state)))
    (dotimes (i rounds state)
      (incf (player-state-round state))
      (format t "~%==============================~%")
      (format t "NIETZSCHE TRIAL — ROUND ~a~%" (1+ i))
      (format t "==============================~%")
      (format t "~%Enter a belief/claim/statement:~%")

      (let* ((input (read-line))
             (result (play-nietzsche-round input)))
        (push result (player-state-history state))

        (format t "~%Reflection (optional): What shifted, if anything?~%")
        (push (read-line) (player-state-notes state))))

    (format t "~%TRIAL COMPLETE. Rounds: ~a~%" (player-state-round state))
    state))

