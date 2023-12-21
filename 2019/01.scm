(import test)
(import srfi-1)
(import (chicken io))

; Read input data from stdin
(define (getModules) (map string->number (read-lines)))
(define modules (getModules))

(define (fuelForMass mass)
  (- (floor (/ mass 3)) 2))

(define (modulesFuel moduleMasses)
  (fold + 0 (map fuelForMass moduleMasses)))

(test 2 (modulesFuel '(12)))
(test 654 (modulesFuel '(1969)))
(test 33583 (modulesFuel '(100756)))

(print "1-1: " (modulesFuel modules))


(define (integrateModuleFuel moduleMass)
  (let ((fuel (fuelForMass moduleMass)))
    (if (< fuel 1)
        0
        (+ fuel (integrateModuleFuel fuel)))))

(define (integratedModulesFuel moduleMasses)
  (fold + 0 (map integrateModuleFuel moduleMasses)))

(test 2 (integratedModulesFuel '(14)))
(test 966 (integratedModulesFuel '(1969)))
(test 50346 (integratedModulesFuel '(100756)))

(print "1-2: " (integratedModulesFuel modules))
