(module Terminal
    (createScreen!
     destroyScreen!
     pushMsg!
     askMsg
     getKey
     drawPlayer!
     drawMap!
     drawPath!)

  (import chicken scheme)
  (use srfi-1 srfi-25 extras ncurses)
  

  (define log '())
  (define mapWin)
  (define logWin)
  (define statWin)

  
  (define (createScreen!)
    (initscr)
    (curs_set 0)
    (noecho)
    (let* ((mapWinWidth 50)
	  (mapWinHeight 20)
	  (statWinWidth 10)
	  (logWinHeight 3)
	  (logWinWidth mapWinWidth)
	  (statWinHeight (+ mapWinHeight logWinHeight)))
      (let-values (((termHeight termWidth) (getmaxyx stdscr)))
	(if (or (< termWidth logWinWidth) (< termHeight statWinHeight))
	    (destroyScreen! (format #f "Terminal size (~a, ~a) too small!\n" termWidth termHeight))
	    (if (not (has_colors))
		(destroyScreen! (format #f "Terminal doesn't support colors"))
		(begin
		  (start_color)
		  (init_pair 1 COLOR_RED COLOR_BLACK)
		  (init_pair 2 COLOR_YELLOW COLOR_BLACK)
		  (set! mapWin (newwin mapWinHeight mapWinWidth 0 0))
		  (set! logWin (newwin logWinHeight logWinWidth mapWinHeight 0))
		  (set! statWin (newwin statWinHeight statWinWidth 0 mapWinWidth))))))))
        
	      
  (define (destroyScreen! msg)
    (endwin)
    (format #t "~a\n" msg))


  (define (getKey)
    (wgetch mapWin))

  (define (pushMsg! msg)
    (define (push! e)
      (begin
	(when (= (length log) 3)
	  (pull!))
	(set! log (cons e log))))
    
    (define (pull!)
      (let ((l (last log)))
	(begin
	  (drop-right! log 1)
	  l)))
    
    (begin
      (if (eq? log '())
	  (push! `(,msg 1))
	  (if (string=? msg (first (first log)))
	      (set-car! log `(,msg ,(+ (second (first log)) 1)))
	      (push! `(,msg 1))))
      
      (wclear logWin)
      
      (let ((y 0))
	(for-each (lambda (msgPair)
		    (let* ((m (first msgPair)) (n (second msgPair))
			   (str (if (> n 1) (format #f "~a (x~a)" m n) (format #f "~a" m))))
		      (mvwaddstr logWin y 0 str)
		      (set! y (+ y 1))))
		  log))

      (wrefresh logWin)))

  
  (define (mvwgetstr win y x) ;there is a bug with the original version, so I rewrote it using getch
    (wmove win y x)
    (let loop ((str ""))
      (let ((ch (wgetch win)))
	(if (eq? (char->integer ch ) 10) ;10 is the ENTER KEY code
	    str
	    (if (<= (char->integer ch) 127)
		(loop str)
		(loop (string-append str (string ch))))))))

  (define (askMsg msg)
    (let ((answer ""))
      (curs_set 1)
      (pushMsg! msg)
      (set! answer (mvwgetstr logWin 0 (string-length msg)))
      (curs_set 0)
      (set-car! log `(,(string-append msg answer) 1))
      answer))

  (define (drawPlayer!)
    (let-values (((height width) (getmaxyx mapWin)))
      (mvwaddch mapWin (quotient height 2) (quotient width 2) #\@)))
  
  (define (drawMap! dungeon fov x y dungeonWidth dungeonHeight)
    (wclear mapWin)
    (let-values (((mapWinHeight mapWinWidth) (getmaxyx mapWin)))
      (for-each (lambda (yScreen)
		  (for-each (lambda (xScreen)
			      (let ((xMap (+ x (- xScreen (quotient mapWinWidth 2))))
				    (yMap (+ y (- yScreen (quotient mapWinHeight 2)))))
				(when (and (< xMap dungeonWidth) (< yMap dungeonHeight)
					   (> xMap 0) (> yMap 0))
				  (when (array-ref fov xMap yMap)
				    (wattron mapWin A_BOLD)
				    (wattron mapWin (COLOR_PAIR 2)))
                  

                  (mvwaddch mapWin yScreen xScreen (array-ref dungeon xMap yMap))
				  (wattroff mapWin A_BOLD)
				  (wattroff mapWin (COLOR_PAIR 2)))))
			    (iota (- mapWinWidth 1))))
		(iota (- mapWinHeight 1))))
    (wrefresh mapWin))


  (define (drawPath! dungeon path x y) ;temporanea
    (when (not (null? path))
      (let-values (((mapWinHeight mapWinWidth) (getmaxyx mapWin)))
	(let* ((cell (car path)) (xMap (first cell)) (yMap (second cell))
	       (xScreen (- (+ xMap (quotient mapWinWidth 2)) x))
	       (yScreen (- (+ yMap (quotient mapWinHeight 2)) y)))
	  (when (and (>= xScreen 0) (>= yScreen 0) (< xScreen (- mapWinWidth 1)) (< yScreen (- mapWinHeight 1)))
	    (wattron mapWin (COLOR_PAIR 1))
	    (mvwaddch mapWin yScreen xScreen #\o)
	    (wattroff mapWin (COLOR_PAIR 1))
	    )
	    (drawPath! dungeon (cdr path) x y)))))
    
				
    

  

  )
