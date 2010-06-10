
(declaim (optimize (debug 3)))

(defun sum (numbers)
  (apply #'+ 0 numbers))

(defun resample (objects weights new-size)
  (when (> new-size 0)
    ;; normalize weights
    (let* ((sum-weights (sum weights))
           (weights (map 'list (lambda (w) (/ w sum-weights)) weights)))
      (let* ((inverse-size (/ 1 new-size))
             (r (* (random 1.0) inverse-size))
             (i 0)
             (c (elt weights 0)))
        (let ((result nil))
          (dotimes (m new-size)
            (let ((u (* (+ r (- m 1.0)) inverse-size)))
              (loop :until (>= c u)
                    :do (setf i (+ i 1)
                              c (+ c (elt weights i))))
              (push (elt objects i) result)))
          result)))))

"def lowVarianceSample2(objects, weights, newSize):
    if newSize > 0:
        sumWeights = float(sum(weights))
        weights = [float(x) / sumWeights for x in weights]
        print sum(weights)
        invSize = 1.0 / float(newSize)
        r = random.random() * invSize
        i = 0
        c = weights[0]
        for m in xrange(0, newSize):
            u = r + float(float(m) - 1.0 ) * invSize
            while u > c:
                i = i + 1
                c = c + weights[i]
            yield objects[i]"