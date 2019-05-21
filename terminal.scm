(declare
 (fixnum-arithmetic)
 (disable-interrupts)
 (foreign-declare "#include <ncurses.h>"))


(module terminal
        (<terminal>
         make-terminal
         destroy-terminal
         get-key
         push-msg
         ask-msg
         draw-map)

        (import scheme
                (chicken foreign)
                srfi-1 ;for lists manipulation
                srfi-28 ;for string formatting
                srfi-25 ;for arrays
                coops
                coops-primitive-objects ;for passing standard chicken types as method arguments
                dungeon
                player)

        ;;;Low level access to ncurses API directly using C
        (define-foreign-type WINDOW "WINDOW")
        (define-foreign-type bool "bool")
        (define-foreign-type short "short")

        (define initscr (foreign-lambda (c-pointer WINDOW) "initscr"))
        (define curs_set (foreign-lambda int "curs_set" int))
        (define echo (foreign-lambda int "echo"))
        (define noecho (foreign-lambda int "noecho"))
        ;;;because getmaxyx is a C macro this ugly approach is needed
        (define getmaxheight (foreign-lambda* int (((c-pointer WINDOW) win))
                                             "int x, y;
                                              getmaxyx(win, y, x);
                                              C_return(y);"))
        (define getmaxwidth (foreign-lambda* int (((c-pointer WINDOW) win))
                                             "int x, y;
                                              getmaxyx(win, y, x);
                                              C_return(x);"))
        (define endwin (foreign-lambda int "endwin"))
        (define newwin (foreign-lambda (c-pointer WINDOW) "newwin" int int int int))
        (define has_colors (foreign-lambda bool "has_colors"))
        (define start_color (foreign-lambda int "start_color"))
        (define init_pair (foreign-lambda int "init_pair" short short short))
        (define COLOR_PAIR (foreign-lambda int "COLOR_PAIR" int))
        (define COLOR_BLACK (foreign-value "COLOR_BLACK" int))
        (define COLOR_RED (foreign-value "COLOR_RED" int))
        (define COLOR_GREEN (foreign-value "COLOR_GREEN" int))
        (define COLOR_YELLOW (foreign-value "COLOR_YELLOW" int))
        (define COLOR_BLUE (foreign-value "COLOR_BLUE" int))
        (define COLOR_MAGENTA (foreign-value "COLOR_MAGENTA" int))
        (define COLOR_CYAN (foreign-value "COLOR_CYAN" int))
        (define COLOR_WHITE (foreign-value "COLOR_WHITE" int))
        (define wattron (foreign-lambda int "wattron" (c-pointer WINDOW) int))
        (define wattroff (foreign-lambda int "wattroff" (c-pointer WINDOW) int))
        (define wclear (foreign-lambda int "wclear" (c-pointer WINDOW)))
        (define wrefresh (foreign-lambda int "wrefresh" (c-pointer WINDOW)))
        (define mvwaddnstr (foreign-lambda int "mvwaddnstr" (c-pointer WINDOW) int int c-string int))
        ;;;mvwgetnstr doesn't return a string natively, so I wrapped it with this simple C code 
        (define mvwgetnstr (foreign-lambda* c-string (((c-pointer WINDOW) win) (int y) (int x) (int n))
                                            "char* str = malloc(sizeof(char) * n);
                                             mvwgetnstr(win, y, x, str, n);
                                             C_return(str);"))
        (define wgetch (foreign-lambda char "wgetch" (c-pointer WINDOW)))
        (define mvwaddch (foreign-lambda int "mvwaddch" (c-pointer WINDOW) int int char))




        ;;;Class representing the terminal screen
        (define-class <terminal> ()
          ((log-msgs initform: '() accessor: log-msgs)
           (map-win initform: '() accessor: map-win)
           (log-win initform: '() accessor: log-win)
           (stat-win initform: '() accessor: stat-win)))

        (define (make-terminal) ;Constructor for <terminal>
          (let* ((t (make <terminal>))
                 (stdscr (initscr))
                 (map-win-width 50)
                 (map-win-height 20)
                 (stat-win-width 10)
                 (log-win-height 3)
                 (log-win-width map-win-width)
                 (stat-win-height (+ map-win-height log-win-height))
                 (term-width (getmaxwidth stdscr))
                 (term-height (getmaxheight stdscr)))

            (if (or (< term-width log-win-width) (< term-height stat-win-height))
                (destroy-terminal t "Terminal too small")
                (if (not (has_colors))
                    (destroy-terminal t "Terminal doesn't support colors")
                    (begin ;go there if the terminal size is correct and it has colors
                      (curs_set 0)
                      (noecho)
                      (start_color)
                      (init_pair 1 COLOR_GREEN COLOR_MAGENTA) ;colors palette definition 
                      (init_pair 2 COLOR_MAGENTA COLOR_GREEN)

                      (set! (map-win t)
                            (newwin map-win-height map-win-width 0 0))
                      (set! (log-win t)
                            (newwin log-win-height log-win-width map-win-height 0))
                      (set! (stat-win t)
                            (newwin stat-win-height stat-win-width 0 map-win-width))
                      )))
            t
            ))

        (define-method (destroy-terminal (t <terminal>) (msg <string>))
          (endwin)
          (format #t "~a\n" msg))

        (define-method (get-key (t <terminal>))
          (wgetch (map-win t)))

        (define-method (push-msg (t <terminal>) (msg <string>))
          (define (push e)
            (begin
              (if (= (length (log-msgs t)) 3)
                (pull))
              (set! (log-msgs t) (cons e (log-msgs t)))))

          (define (pull)
            (let ((l (last (log-msgs t))))
              (begin
                (drop-right! (log-msgs t) 1)
                l)))

          (begin
            (if (eq? (log-msgs t) '())
                (push `(,msg 1))
                (if (string=? msg (first (first (log-msgs t))))
                    (set-car! (log-msgs t) `(,msg ,(+ (second (first (log-msgs t))) 1)))
                    (push `(,msg 1))))

            (wclear (log-win t))

            (let ((y 0))
              (for-each (lambda (msg-pair)
                          (let* ((m (first msg-pair)) (n (second msg-pair))
                                 (str (if (> n 1) (format #f "~a (x~a)" m n) (format #f "~a" m))))
                            (mvwaddnstr (log-win t) y 0 str (getmaxwidth (log-win t)))
                            (set! y (+ y 1))))
                        (log-msgs t)))

            (wrefresh (log-win t))))

        (define-method (ask-msg (t <terminal>) (msg <string>) (ans-len <integer>))
          (let ((answer ""))
            (echo)
            (curs_set 1)
            (push-msg t msg)
            (set! answer (mvwgetnstr (log-win t) 0 (string-length msg) ans-len))
            (curs_set 0)
            (noecho)
            (set-car! (log-msgs t) `(,(string-append msg answer) 1))
            answer))

        (define-method (draw-map (t <terminal>) (d <dungeon>) (p <player>))
          (wclear (map-win t))
          (let ((map-win-width (getmaxwidth (map-win t)))
                (map-win-height (getmaxheight (map-win t))))
            (for-each (lambda (y-screen)
                        (for-each (lambda (x-screen)
                                    (let ((x-map (+ (get-x p) (- x-screen (quotient map-win-width 2))))
                                          (y-map (+ (get-y p) (- y-screen (quotient map-win-height 2)))))
                                      (if (and (< x-map (get-width d)) (< y-map (get-height d))
                                               (> x-map 0) (> y-map 0))
                                          (begin
                                            (if (and (= (get-x p) x-map) (= (get-y p) y-map))
                                                (mvwaddch (map-win t) y-screen x-screen #\@)
                                            
                                        ;(when (eq? (array-ref dungeon xMap yMap) #\#)
                                        ;  (wattron mapWin (COLOR_PAIR 5)))
                                        
                                        ;(when (array-ref fov xMap yMap) 
                                        ;  (wattron mapWin A_BOLD)
                                        ;(when (eq? (array-ref dungeon xMap yMap) #\#)
                                        ;    (wattron mapWin (COLOR_PAIR 2)))
                                        ;  (when (eq? (array-ref dungeon xMap yMap) #\.)
                                        ;    (wattron mapWin (COLOR_PAIR 4))))
                                        
                                        
                                            (mvwaddch (map-win t) y-screen x-screen (array-ref (get-grid d) x-map y-map)))
                                        ;(wattroff mapWin A_BOLD)
                                        ;(wattroff mapWin (COLOR_PAIR 4))
                                        ;(wattroff mapWin (COLOR_PAIR 5))
                                        ;(wattroff mapWin (COLOR_PAIR 2)))))
                                            ))))
                                  (iota (- map-win-width 1))))
                      (iota (- map-win-height 1))))
          (wrefresh (map-win t)))
        



                        )
