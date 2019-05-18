(declare
 (fixnum-arithmetic)
 (disable-interrupts)
 (foreign-declare "#include <ncurses.h>"))


(module terminal
        (make-terminal
         destroy-terminal)

        (import scheme
                (chicken foreign)
                coops
                coops-primitive-objects)

        ;Low level access to ncurses API directly using C
        (define-foreign-type WINDOW "WINDOW")
        (define-foreign-type bool "bool")
        (define-foreign-type short "short")

        (define initscr (foreign-lambda (c-pointer WINDOW) "initscr"))
        (define curs_set (foreign-lambda int "curs_set" int))
        (define noecho (foreign-lambda int "noecho"))
        ;because getmaxyx is a C macro this ugly approach is needed
        (define getmaxheight (foreign-lambda* int (((c-pointer WINDOW) win))
                                             "int x, y;
                                              getmaxyx(win, y, x);
                                              C_return(y);"))
        (define getmaxwidth (foreign-lambda* int (((c-pointer WINDOW) win))
                                             "int x, y;
                                              getmaxyx(win, y, x);
                                              C_return(x);"))
        (define endwin (foreign-lambda int "endwin"))
        (define has_colors (foreign-lambda bool "has_colors"))
        (define start_color (foreign-lambda int "start_color"))
        (define init_pair (foreign-lambda int "init_pair" short short short))
        (define COLOR_PAIR (foreign-lambda int "COLOR_PAIR" int))
        (define COLOR_RED (foreign-value "COLOR_RED" int))
        (define COLOR_GREEN (foreign-value "COLOR_GREEN" int))
        (define COLOR_MAGENTA (foreign-value "COLOR_MAGENTA" int))
        (define wattron (foreign-lambda int "wattron" (c-pointer WINDOW) int))
        (define wattroff (foreign-lambda int "wattroff" (c-pointer WINDOW) int))
        (define getch (foreign-lambda int "getch"))
        (define mvwaddch (foreign-lambda int "mvwaddch" (c-pointer WINDOW) int int char))




        ;Class representing the terminal screen
        (define-class <terminal> ()
          ((log '())
           (map-win '())
           (log-win '())
           (stat-win '())))

        (define (make-terminal)
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


                )))
            t
            ))

        (define-method (destroy-terminal (t <terminal>) (msg <string>))
          (endwin)
          (display msg))

)
