(in-package :de.anvi.girc)

;; https://www.rfc-editor.org/rfc/rfc4648
(defun base64-encode-octets (b1 &optional b2 b3)
  "Take 1 to 3 8-bit octets and return 4 base64-encoded 6-bit characters."
  (let ((dict "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (mask (logior (ash (if b1 b1 0) 16)
                      (ash (if b2 b2 0)  8)
                      (ash (if b3 b3 0)  0))))
    (list (char dict (ash (logand (ash #b111111 18) mask) -18))
          (char dict (ash (logand (ash #b111111 12) mask) -12))
          ;; If octets b2 and b3 are missing, pad the result with 1 or 2 "=" chars.
          (if b2
              (char dict (ash (logand (ash #b111111  6) mask) -6))
              #\=)
          (if b3
              (char dict (ash (logand (ash #b111111  0) mask) 0))
              #\=))))

(defun base64-encode-string (str)
  "Return str encoded in Base64 6-bit characters.

If str contains multibyte characters, convert them to UTF-8 octets first."
  (let ((octets (mapcan (lambda (ch)
                          (crt::unicode-to-utf-8 (char-code ch)))
                        (coerce str 'list)))
        result)
    (loop for (b1 b2 b3) on octets by #'cdddr do
      (mapc (lambda (ch)
              (push ch result))
            (base64-encode-octets b1 b2 b3)))
    (coerce (nreverse result) 'string)))

(defun debug-fmtbin (int)
  "Format the integer int as a 24-bit binary used for Base64 encoding."
  (format t "~24,'0b" int))

;; https://www.rfc-editor.org/rfc/rfc4616
;; message = [authzid] UTF8NUL authcid UTF8NUL passwd
(defun sasl-plain-token (authzid authcid passwd)
  (format nil "~A~C~A~C~A" authzid #\nul authcid #\nul passwd))
