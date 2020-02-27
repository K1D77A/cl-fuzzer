;;;;this file contains all the functions required to generate strings

(in-package :cl-fuzzer)


;;imagine an input like "[a3][b3][c3][1..3]" would return "aaabbbccc123" etc
;;imagine an input like '((3 "ab32a")(24 :null)(1 25))
(defparameter *test-input*
  '((:string "start")(:repeat 3 "ab32a")(:repeat 24 :nul)(:repeat 10 :LF)(:upto 1 25)(:downto 25 0)
    (:char 1 #\Newline)(:random-string 5)(:string "stop")))


(defparameter *special-asciis*
  '((:NUL #\Nul) (:SOH #\Soh) (:STX #\Stx) (:ETX #\Etx) (:EOT #\Eot) (:ENQ #\Enq)
    (:ACK #\Ack) (:BEL #\Bel) (:BS #\Backspace) (:TAB #\Tab) (:LF #\Newline)
    (:VT #\Vt) (:FF #\Page) (:CR #\Return) (:SO #\So) (:SI #\Si) (:DLE #\Dle)
    (:DC1 #\Dc1) (:DC3 #\Dc2) (:DC3 #\Dc3) (:DC4 #\Dc4) (:NAK #\Nak) (:SYN #\Syn)
    (:ETB #\Etb) (:CAN #\Can) (:EM #\Em) (:SUB #\Sub) (:ESC #\Esc) (:FS #\Fs)
    (:GS #\Gs) (:RS #\Rs) (:US #\Us) (:SPACE #\ )(:DEL #\Rubout)))
(defun random-string-list (len)
  (let ((chars "abcdefghijklmnopqrstuvwxyz"))
    (map 'list #'char-code
         (loop :for x :from 0 :to len
               :collect (aref chars (random 25))))))
(defun string-repeat (n string)
  "repeats string n times"
  (let ((str ""))
    (map 'list #'char-code
         (dotimes (x n str)
           (setf str (concatenate 'string str string))))))
(defun special-char-repeat (n char)
  (string-repeat n (string char)))
(defun n..x (n x)
  "Generates the numbers from n upto x. n must be greater than 0 and n must be less than 255"
  (if (and (>= n 0) (<= x 255) (< n x))
      (loop :for i :from n :to x
            :collect i)
      (error "N is less than 0 or X is greater than 255")))
(defun x..n (x n)
  "Generates the numbers from x downto n. n must be greater than 0 and n must be less than 255"
  (if (and (>= x 0) (<= n 255)(> x n))
      (loop :for i :from x :downto n
            :collect i)
      (error "N is less than 0 or X is greater than 255")))
(defun handle-repeat (form)
  (destructuring-bind (n repeat)
      form
    (if (keywordp repeat)
        (special-char-repeat n (second  (assoc repeat *special-asciis*)))
        (string-repeat n repeat))))

(defun handle-upto (form)
  (destructuring-bind (x y)
      form
    (n..x x y)))
(defun handle-downto (form)
  (destructuring-bind (x y)
      form
    (x..n x y)))
(defun handle-string (form)
  (destructuring-bind (string)
      form
    (string-repeat 1 string)))
(defun handle-char (form)
  (destructuring-bind (n char)
      form
    (string-repeat n (string char))))
(defun handle-random-string (form)
  (destructuring-bind (n)
      form
    (random-string-list n)))
(defun generate-fuzzy-list (form)
  (reduce #'append
          (mapcar (lambda (form)
                    (let ((command (first form)))
                      (case command
                        (:string (handle-string (rest form)))
                        (:char (handle-char (rest form)))
                        (:repeat (handle-repeat (rest form)))
                        (:upto (handle-upto (rest form)))
                        (:downto (handle-downto (rest form)))
                        (:random-string (handle-random-string (rest form))))))
                  form)))
(defun fuzzy-list-to-fuzzy-string (fuzzy-list)
  (map 'string #'code-char fuzzy-list))




