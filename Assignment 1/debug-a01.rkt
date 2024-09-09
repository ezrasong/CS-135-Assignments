;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname debug-a01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))


(define (luminosity red-pixel\_value green-pixel\_value blue-pixel\_value) 0.3 * red-pixel\_value + 0.59 * green-pixel\_value + 0.11 * blue-pixel\_value)

