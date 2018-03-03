;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-project2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define MTSCREEN (rectangle WIDTH HEIGHT "outline" "white"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 1.5))           ;not landed, moving right
(define I2 (make-invader 150 26 -1.5))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ 80 10) 1.5)) ;> landed, moving right
(define I4 (make-invader 100 15 -1.5))
(define I5 (make-invader 200 30 1.5))


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListofInvader is (cons (invader empty))
;; Interp. ListofInvader is one of:
;; - empty
;; - invader ListofInvader
#;
(define (fn-for-loinvader LOI)
  (cond [(empty?) empty]
        [(...) (first LOI)
               (fn-for-loinvader (rest LOI))]))

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I3 (cons I2 (cons I1 empty))))
(define LOI4 (cons I5 (cons I4 empty)))

;; ListofMissile is (cons (missile empty))
;; Interp. ListofMissile is one of:
;; - empty
;; - missile ListofMissile

#;
(define (fn-for-lomissile LOM)
  (cond [(empty?) empty]
        [(...) (first LOm)
               (fn-for-lomissile (rest LOM))]))

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons M1 empty)))


;; Game examples, since all lists are now defined

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game LOI4 LOM1 T0))
(define G5 (make-game LOI2 LOM1 T0))
(define G6 (make-game LOI3 LOM1 T0))


;;================================
;; Functions
;;================================

;; Game -> Game
;; Consumes a worldstate and generates the next one

(define (main s)
  (big-bang s
            (on-tick   movePieces)
            (to-draw   renderPieces)
            (on-key    tankControls)))



;;=================================
;; Movement functions (long)
;;=================================


;; Game -> Game
;; Interp. Consumes a game state and produces the next game state by moving the pieces

;(define (movePieces s) 0)  ; stub

  

#;
(check-expect (movePieces G4) (make-game (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty))
                                         (cons (make-missile 150 290) empty)
                                         (make-tank (+ (/ WIDTH 2) (* (tank-dir T0) TANK-SPEED)) 1)))



(define (movePieces s)
  (make-game
   (sepInvader (randomGenerate (hitDetection (game-invaders s) (game-missiles s))))
   (sepMissile (game-missiles s))
   (moveTank (game-tank s))))


;; ListofInvaders ListofMissile -> ListofInvaders
;; Interp. checks the positions of the invaders and missiles in their respective lists and removes an invader if it has been hit by a missile

;(define (hitDetection LOII LOMM) empty) ;stub


(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 150 290) empty))
              (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)))

(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 201.5 290) empty))
              (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)))

(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 150 16.5) empty))
              (cons (make-invader 201.5 31.5 1.5) empty))

(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 148.5 16.5) empty))
              (cons (make-invader 201.5 31.5 1.5) empty))

(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 201.5 290) (cons (make-missile 148.5 16.5) empty)))
              (cons (make-invader 201.5 31.5 1.5) empty))

(check-expect (hitDetection (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 148.5 16.5 -1.5) empty)) (cons (make-missile 201.5 31.5) (cons (make-missile 37 16.5) empty)))
              (cons (make-invader 148.5 16.5 -1.5) empty))


(define (hitDetection LOII LOMM)
  (cond [(empty? LOII) LOII]
        [(comparePos LOII LOMM)
         (remove (first LOII) LOII)]
        [(not (empty? LOMM))
         (hitDetection LOII (rest LOMM))]
        [else
         (cons (first LOII) (hitDetection (rest LOII) LOMM))]))


;(define (hitDetection LOII LOMM)
;  (cond [(comparePos LOII LOMM)
;         (remove (first LOII) LOII)]
;        [(not (empty? LOMM))
;         (hitDetection LOII (rest LOMM))]
;        [(not (empty? LOII))
;         (hitDetection (rest LOII) LOMM)]
;        ;        [(comparePos LOII (rest LOMM))
;        ;         (remove (first LOII) LOII)]
;        ;        [(comparePos (rest LOII) LOMM)
;        ;         (remove (first (rest LOII)) LOII)]
;        ;        [(comparePos (rest LOII) (rest LOMM))
;        ;         (remove (first (rest LOII)) LOII)]
;        ;        [(and (not (empty? (rest LOII))) (not (empty? (rest LOMM))))
;        ;         (hitDetection (rest LOII) (rest LOMM))]
;        ;        [(and (not (empty? (rest LOII))) (empty? (rest LOMM)))
;        ;         (hitDetection (rest LOII) LOMM)]
;        ;        [(and (empty? (rest LOII)) (not (empty? (rest LOMM))))
;        ;         (hitDetection LOII (rest LOMM))]
;        [else LOII]))


(define (comparePos LOI LOM)
  (cond [(empty? LOI) false]
        [(empty? LOM) false]
        [(equal? (invader-x (first LOI)) (missile-x (first LOM)))
         (cond [(equal? (invader-y (first LOI)) (missile-y (first LOM)))
                true]
               [else false])]
        [(and (> (+ (invader-x (first LOI)) 5) (missile-x (first LOM))) (< (- (invader-x (first LOI)) 5) (missile-x (first LOM))))
         (cond [(and (> (+ (invader-y (first LOI)) 5) (missile-y (first LOM))) (< (- (invader-y (first LOI)) 5) (missile-y (first LOM))))
                true]
               [else false])]
        [else false]))



;; ListofInvader -> ListofInvader
;; Interp.  Consumes a ListofInvader and may randomly add a new invader to the list, returning the list either with the addition or unchanged

;(define (randomGenerate LOI) LOI)  ; stub

(define (randomGenerate LOI)
  (cond [(> 7 (random 1000))
         (if (< 50 (random 100))
             (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) LOI)
             (cons (make-invader (random WIDTH) 0 (* -1 INVADER-X-SPEED)) LOI))]
        [else LOI]))







;; ListofInvader -> ListofInvader
;; Interp. Consumes a ListofInvader and moves each invader to the next location based on its current position and direction of movement
(check-expect (sepInvader LOI4) (cons (make-invader 201.5 31.5 1.5) (cons (make-invader 98.5 16.5 -1.5) empty)))

;(define (sepInvader LOI) empty) ;stub

(define (sepInvader LOI)
  (cond [(empty? LOI) empty]
        [(> (invader-y (first LOI)) HEIGHT)
         (remove (first LOI) LOI)]
        [else (cons (moveInvader (first LOI))
                    (sepInvader (rest LOI)))]))



(define (moveInvader i)
  (cond [(> 0 (invader-dx i))
         (moveInvaderNegSpeed i)]
        [else
         (moveInvaderPosSpeed i)]))



(define (moveInvaderNegSpeed i)
  (cond [(>= 3 (invader-x i)) (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) (invader-dx i)) INVADER-X-SPEED)]
        [else (make-invader (- (invader-x i) INVADER-X-SPEED) (- (invader-y i) (invader-dx i)) (invader-dx i))]))



(define (moveInvaderPosSpeed i)
  (cond [(<= (- WIDTH 3) (invader-x i)) (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) (invader-dx i)) (* -1 INVADER-X-SPEED))]
        [else (make-invader (+ (invader-x i) INVADER-X-SPEED) (+ (invader-y i) (invader-dx i)) (invader-dx i))]))



;; ListofMissile -> ListofMissile
;; Consumes a ListofMissile and moves each missile to the next location based on missile speed constant
;(define (sepMissile LOM) 0)
(check-expect (sepMissile LOM2) (cons (make-missile 150 290) empty))

(define (sepMissile LOM)
  (cond [(empty? LOM) empty]
        [(< (missile-y (first LOM)) 0)
         (remove (first LOM) LOM)]
        [else
         (cons (moveMissile (first LOM))
               (sepMissile (rest LOM)))]))


;; Missile -> Missile
;; Interp. Consumes a missile and moves it to the next location based on its current position and speed


;(define (moveMissile m l) 0)
(check-expect (moveMissile M1) (make-missile (missile-x M1) 290))

(define (moveMissile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))


;; Tank -> Tank
;; Interp. Consumes a tank and changes its x-coord left or right depending on the direction of its movement


; (define (moveTank t) 0) ; stub
(check-expect (moveTank T0) (make-tank (+ (/ WIDTH 2) (* (tank-dir T0) TANK-SPEED)) 1))
(check-expect (moveTank T2) (make-tank (+ 50 (* (tank-dir T2) TANK-SPEED)) -1))


(define (moveTank t)
  (cond [(= 0 (tank-x t)) (make-tank (+ (tank-x t) (* (tank-dir t) (- 1 TANK-SPEED)) 5) 1)]
        [(= WIDTH (tank-x t)) (make-tank (+ (tank-x t) (* (tank-dir t) (- 1 TANK-SPEED)) -5) -1)]
        [else (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))




;;=============================
;; Rendering Functions
;;=============================

;(define (renderPieces p) 0)

(define (renderPieces p)
  (overlay
   (sepInvaderPics (game-invaders p))
   (sepMissilePics (game-missiles p))
   (renderTank (game-tank p))))

(define (sepInvaderPics LIPic)
  (cond [(empty? LIPic) (place-image (rectangle 0 0 "solid" "white") 0 0 MTSCREEN)]
        [else (overlay
               (renderInvader (first LIPic))
               (sepInvaderPics (rest LIPic)))]))

(define (renderInvader IPic)
  (place-image INVADER (invader-x IPic) (invader-y IPic) MTSCREEN))


(define (sepMissilePics LMPic)
  (cond [(empty? LMPic) (place-image (rectangle 0 0 "solid" "white") 0 0 MTSCREEN)]
        [else (overlay
               (renderMissile (first LMPic))
               (sepMissilePics (rest LMPic)))]))

(define (renderMissile MPic)
  (place-image MISSILE (missile-x MPic) (missile-y MPic) MTSCREEN))

(define (renderTank TPic)
  (place-image TANK (tank-x TPic) (- HEIGHT TANK-HEIGHT/2) MTSCREEN))



;;============================
;; Key handler Functions
;;============================


(define (tankControls tc ke)
  (cond [(key=? ke "left") (make-game (game-invaders tc) (game-missiles tc) (changeTankDirLeft (game-tank tc)))]
        [(key=? ke "right") (make-game (game-invaders tc) (game-missiles tc) (changeTankDirRight (game-tank tc)))]
        [(key=? ke " ") (make-game (game-invaders tc) (createMissile (game-missiles tc) (game-tank tc)) (game-tank tc))]
        [else tc]))

(define (changeTankDirLeft Tke)
  (make-tank (tank-x Tke) -1))

(define (changeTankDirRight Tke)
  (make-tank (tank-x Tke) 1))

(define (createMissile Mke T)
  (cons (make-missile (tank-x T)(- HEIGHT TANK-HEIGHT/2)) Mke))