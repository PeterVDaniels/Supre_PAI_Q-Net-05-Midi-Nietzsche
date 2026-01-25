(defpackage :peteai.nietzsche
  (:use :cl)
  (:export :nietzsche-deliberate))

(in-package :peteai.nietzsche)

(defun %has-any (s needles)
  (some (lambda (n) (search n s :test #'char-equal)) needles))

(defun nietzsche-deliberate (input &key (return :plist))
  "Analyze INPUT in a Nietzsche/genealogical style.
Returns a plist by default: (:flags (...) :questions (...) :notes (...))."
  (let* ((s (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return) input)))
         (flags '())
         (notes '())
         (questions '()))

    ;; crude but useful detectors
    (when (%has-any s '("must" "should" "ought" "have to" "needs to"))
      (push :slave-morality flags)
      (push "Imperative moral language detected — possible moralizing pressure." notes))

    (when (%has-any s '("everyone" "all people" "humanity" "always" "never"))
      (push :herd-appeal flags)
      (push "Universal framing detected — may conceal tradeoffs or coercion." notes))

    (when (%has-any s '("they" "them" "those people" "the rich" "the poor" "blame"))
      (push :resentment flags)
      (push "Group-targeted framing detected — check for resentment/score-settling." notes))

    (when (%has-any s '("deny" "suppress" "ban" "forbid" "eliminate desire"))
      (push :life-denial flags)
      (push "Negation/ban framing detected — check for life-denial vs moderation." notes))

    ;; standard Nietzschean probes
    (setf questions
          (list
           "Who benefits from this 'should/must' framing?"
           "Is this necessity—or a moral whip?"
           "Is it life-enhancing, or obedience-enhancing?"))

    (setf flags (nreverse (remove-duplicates flags)))
    (setf notes (nreverse notes))

    (ecase return
      (:plist (list :flags flags :questions questions :notes notes))
      (:alist `((:flags . ,flags) (:questions . ,questions) (:notes . ,notes))))))

