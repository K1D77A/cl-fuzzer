# cl-fuzzer


This project is a simple stream fuzzer. You generate random sequences and then jam them down a stream to test the resilience of the receiver.

## License

MIT

## Current doc
Generating random characters is quite easy. You simply pass an alist of keywords and args to (generate-fuzzy-list ..).
Assume that a variable called *test-input* has been made
```lisp
(defparameter *test-input*
  '((:string "start")(:repeat 3 "ab32a")(:repeat 24 :nul)(:repeat 10 :LF)(:upto 1 25)(:downto 25 0)
    (:char 1 #\Newline)(:random-string 5)(:string "stop")))
    
```
Now call (generate-fuzzy-list \*test-input\*)
```lisp
CL-FUZZER> (generate-fuzzy-list *test-input*)
(115 116 97 114 116 97 98 51 50 97 97 98 51 50 97 97 98 51 50 97 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 10 10 10 10 10 10 10 10 10 10 1 2 3 4 5 6 7
 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 25 24 23 22 21 20 19 18 17
 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0 10 115 116 111 112)
```

easy.
The current supported keywords and what they do are:
```lisp
:string <string> places the string as is
:repeat <n> <string> repeats string n times also accepts a special keyword representing an ascii 
character
:upto <n> <x> generates numbers from n to x. n must be 0 or greater, and x must be 255 or lower and greater than n
:downto <x> <n> the opposite of upto, so generates from x downto n
:char <n> <char> repeats Char n times
:randomstring <n> generates a random string from the alphabet of length n
```

The keywords to characters translation is:
```lisp
'((:NUL #\Nul) (:SOH #\Soh) (:STX #\Stx) (:ETX #\Etx) (:EOT #\Eot) (:ENQ #\Enq)
    (:ACK #\Ack) (:BEL #\Bel) (:BS #\Backspace) (:TAB #\Tab) (:LF #\Newline)
    (:VT #\Vt) (:FF #\Page) (:CR #\Return) (:SO #\So) (:SI #\Si) (:DLE #\Dle)
    (:DC1 #\Dc1) (:DC3 #\Dc2) (:DC3 #\Dc3) (:DC4 #\Dc4) (:NAK #\Nak) (:SYN #\Syn)
    (:ETB #\Etb) (:CAN #\Can) (:EM #\Em) (:SUB #\Sub) (:ESC #\Esc) (:FS #\Fs)
    (:GS #\Gs) (:RS #\Rs) (:US #\Us) (:SPACE #\ )(:DEL #\Rubout)))
```
As described at [asciitables](https://www.asciitable.com/)

