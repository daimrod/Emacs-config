;;; web-test.el --- tests for the web client

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: hypermedia, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is another attempt at an HTTP client in EmacsLisp.

;;; Code:

(require 'web)
(require 'elnode)
(require 'fakir)

(ert-deftest web--to-query-string ()
  "Test query string making."
  (let ((t1 #s(hash-table size 5 data (a 1 b 2 c 3 d "str" e t))))
    (should
     (equal "a=1&b=2&c=3&d=str&e"
            (web--to-query-string t1))))
  (let ((t2 '((a . 1)("b" . 2)(c . 3)(d . "str")(e . t))))
    (should
     (equal "a=1&b=2&c=3&d=str&e"
            (web--to-query-string t2)))))

(ert-deftest web-header-parse ()
  "Test HTTP header parsing."
  (let ((hdrs (web-header-parse
               "HTTP/1.0 200 Ok\r
Content-type: text/html\r
Content-length: 1000\r
")))
    (should (equal "1.0" (gethash 'status-version hdrs)))
    (should (equal "200" (gethash 'status-code hdrs)))
    (should (equal "Ok" (gethash 'status-string hdrs)))
    (should (equal "text/html" (gethash 'content-type hdrs)))
    (should (equal "1000" (gethash 'content-length hdrs))))
  (let ((hdrs (web-header-parse
               "HTTP/1.0 400\r
Content-type: text/html\r
Content-length: 1000\r
")))
    (should (equal "1.0" (gethash 'status-version hdrs)))
    (should (equal "400" (gethash 'status-code hdrs)))
    (should (equal "" (gethash 'status-string hdrs)))
    (should (equal "text/html" (gethash 'content-type hdrs)))
    (should (equal "1000" (gethash 'content-length hdrs)))))

(ert-deftest web--chunked-decode-stream ()
  "Test the chunked decoding."
  ;; Test incomplete chunk delivered (missing trailing crlf)
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process :fake ()
          (progn
            (should-not
             (equal
              :done
              (web--chunked-decode-stream
               proc "b\r\nhello world" 'consumer)))
            (should
             (equal "b\r\nhello world"
                    (process-get proc :chunked-encoding-buffer)))
            (should
             (equal
              :done
              (web--chunked-decode-stream
               proc "\r\n0\r\n\r\n" 'consumer)))))))
  ;; Test incomplete chunk packet delivered
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process :fake ()
          (progn
            (should-not
             (equal
              :done
              (web--chunked-decode-stream
               proc "b\r\nhello wor" 'consumer)))
            (should
             (equal "b\r\nhello wor"
                    (process-get proc :chunked-encoding-buffer)))))))
  ;; Test more than 1 complete chunk delivered
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process :fake ()
          (progn
            (should
             (equal :done
                    (web--chunked-decode-stream
                     proc
                     "6\r\nhello!\r\nb\r\nhello world\r\n0\r\n\r\n"
                     'consumer)))
            (should
             (equal "hello!hello world" res))))))
  ;; Test one call handling one chunk and then the end
  (let ((proc :fake)
        (res ""))
    (flet ((consumer (con data)
             (unless (eq data :done)
               (setq res (concat res data)))))
      (fakir-mock-process :fake ()
          (progn
            (should
             (equal :done
                    (web--chunked-decode-stream
                     proc "5\r\nhello\r\n0\r\n\r\n" 'consumer)))
            (should
             (equal "hello" res)))))))

(ert-deftest web-http-post-filter ()
  "Test the filter in streaming mode."
  (let* (cb-hdr
         cd-data
         (con :fake)
         (callback (lambda (con hdr data)
                     (unless cb-hdr
                       (setq cb-hdr hdr))
                     (unless (eq data :done)
                       (setq cb-data data)))))
    (fakir-mock-process :fake ((:buffer "HTTP/1.1 200\r
Host: hostname\r
Transfer-encoding: chunked\r\n"))
      (should-not cb-hdr)
      (web--http-post-filter con "\r\n" callback 'stream)
      ;; Because there is no data yet the header is not set
      (should-not cb-hdr)
      ;; Now send a valid chunk through the stream api
      (web--http-post-filter
       con "b\r\nhello world\r\n" callback 'stream)
      (should cb-hdr)
      (should (equal cb-data "hello world"))
      ;; Some header tests
      (should
       (equal "hostname" (gethash 'host cb-hdr)))
      (should
       (equal "200" (gethash 'status-code cb-hdr)))
      (should
       (equal "1.1" (gethash 'status-version cb-hdr)))
      ;; Now send the final one and catch deleted
      (should
       (eq
        :mock-process-finished
        (catch :mock-process-finished
          (web--http-post-filter con "0\r\n\r\n" callback 'stream)
          (should (equal cb-data "hello world"))))))))

(ert-deftest web-http-post-filter-batch-mode-content-length ()
  "Test the filter in batch mode with fixed content-length."
  (let* (cb-hdr
         cd-data
         (con :fake)
         (callback (lambda (con hdr data)
                     (setq cb-hdr hdr)
                     (setq cb-data data))))
      (fakir-mock-process :fake ((:buffer "HTTP/1.1 200\r
Host: hostname\r
Content-length: 11\r\n"))
        (should-not cb-hdr)
        (web--http-post-filter con "\r\n" callback 'batch)
        (should-not cb-hdr)
        (should
         (eq
          :mock-process-finished
          (catch :mock-process-finished
            (web--http-post-filter con "hello world" callback 'batch)
            (should cb-hdr))))
        (should
         (equal "hostname"
                (gethash 'host cb-hdr)))
        (should
         (equal "200"
                (gethash 'status-code cb-hdr)))
        (should
         (equal "1.1"
                (gethash 'status-version cb-hdr))))))

(ert-deftest web-http-post-filter-batch-mode-chunked ()
  "Test the filter in batch mode with chunked encoding."
  (let* (cb-hdr
         cb-data
         (con :fake)
         (callback (lambda (con hdr data)
                     (setq cb-hdr hdr)
                     (setq cb-data data))))
    (fakir-mock-process :fake ((:buffer "HTTP/1.1 200\r
Transfer-encoding: chunked\r
Host: hostname\r\n"))
      (should-not cb-hdr)
      (web--http-post-filter con "\r\n" callback 'batch)
      (should-not cb-hdr)
      (web--http-post-filter
       con "b\r\nhello world" callback 'batch)
      (should-not cb-hdr)
      (should-not cb-data)
      (should
       (eq
        :mock-process-finished
        (catch :mock-process-finished
          (web--http-post-filter
           con "\r\n0\r\n\r\n" callback 'batch)
          (should cb-hdr)
          (should (equal "hello world" cb-data)))))
      (should
       (equal "hostname" (gethash 'host cb-hdr)))
      (should
       (equal "200" (gethash 'status-code cb-hdr)))
      (should
       (equal "1.1" (gethash 'status-version cb-hdr))))))

(ert-deftest web-http-post-full ()
  "Do a full test of the client using an elnode server.

This tests the parameter passing by having an elnode handler "
  (let* (method
         path
         params
         the-end
         data-received
         (port (elnode-find-free-service)))
    ;; Start a server on the port
    (unwind-protect
         (let ((init-data (make-hash-table
                           :test 'equal
                           :size 5)))
           (puthash "a" 10 init-data)
           (puthash "b" 20 init-data)
           ;; Start the server
           (elnode-start
            (lambda (httpcon)
              (setq method (elnode-http-method httpcon))
              (setq path (elnode-http-pathinfo httpcon))
              (setq params (elnode-http-params httpcon))
              (message "the proc buffer is: %s" (process-buffer httpcon))
              (elnode-http-start httpcon 200 '(Content-type . "text/plain"))
              (elnode-http-return httpcon "hello world!"))
            :port port)
           ;; POST some parameters to the server
           (web-http-post
            (lambda (con header data)
              (setq data-received data)
              (message "data received is: %s" data-received)
              (setq the-end t))
            "/"
            :port port
            :data init-data)
           ;; Hang till the client callback finishes
           (while (not the-end)
             (sit-for 1)))
      ;; And when we're done with the server...
      (elnode-stop port))
    ;; Now test the data that was POSTed and collected inside the
    ;; elnode handler
    (should (equal "POST" method))
    (should
     (equal
      '(("a" . "10")("b" . "20"))
      (sort params
            (lambda (a b)
              (string-lessp (car a) (car b))))))
    ;; And a quick check of the clients receipt of the data from the handler
    (should (equal "hello world!" data-received))))

(provide 'web)

;;; web.el ends here
