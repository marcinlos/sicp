;; Exercise 2.25
;
;   _____ _____      _____ _____      _____ _____                     _____ _____
;  |     |     |    |     |     |    |     |     |                   |     |     |
;  |  *  |  * ----->|  *  |  * ----->|  *  |  * -------------------->|  *  |  X  |
;  |__|__|_____|    |__|__|_____|    |__|__|_____|                   |__|__|_____|
;     |                |                |                               |
;     V                V              __V__ _____      _____ _____      V
;   .---.            .---.           |     |     |    |     |     |   .---.
;   | 1 |            | 3 |           |  *  |  * ----->|  *  |  X  |   | 9 |
;    ---              ---            |__|__|_____|    |__|__|_____|    ---
;                                       |                |
;                                       V                V
;                                     .---.            .---.
;                                     | 5 |            | 7 |
;                                      ---              ---
;
(define x '(1 3 (5 7) 9))
(car (cdr (car (cdr (cdr x)))))


;   _____ _____
;  |     |     |
;  |  *  |  X  |
;  |__|__|_____|
;     |
;   __V__ _____
;  |     |     |
;  |  *  |  X  |
;  |__|__|_____|
;     |
;     V
;   .---.
;   | u |
;    ---
;
(define x '((7)))
(caar x)

;   _____ _____     _____ _____
;  |     |     |   |     |     |
;  |  *  |  * ---->|  *  |  X  |
;  |__|__|_____|   |__|__|_____|
;     |               |
;     V             __V__ _____     _____ _____
;   .---.          |     |     |   |     |     |
;   | 1 |          |  *  |  * ---->|  *  |  X  |
;    ---           |__|__|_____|   |__|__|_____|
;                     |               |
;                     V             __V__ _____     _____ _____
;                   .---.          |     |     |   |     |     |
;                   | 2 |          |  *  |  * ---->|  *  |  X  |
;                    ---           |__|__|_____|   |__|__|_____|
;                                     |               |
;                                     V             __V__ _____     _____ _____
;                                   .---.          |     |     |   |     |     |
;                                   | 3 |          |  *  |  * ---->|  *  |  X  |
;                                    ---           |__|__|_____|   |__|__|_____|
;     .----------------------------------------.       |               |
;     |                                        |       V               |
;   __V__ _____     _____ _____                |     .---.             |
;  |     |     |   |     |     |               |     | 4 |             |
;  |  *  |  * ---->|  *  |  X  |               |      ---              |
;  |__|__|_____|   |__|__|_____|               *-----------------------*
;     |               |
;     V             __V__ _____     _____ _____
;   .---.          |     |     |   |     |     |
;   | 5 |          |  *  |  * ---->|  *  |  X  |
;    ---           |__|__|_____|   |__|__|_____|
;                     |               |
;                     V               V
;                   .---.           .---.
;                   | 6 |           | 7 |
;                    ---             ---
;
(define x '(1 (2 (3 (4 (5 (6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr x))))))