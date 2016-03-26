;;; library for Linux (currently - Feb 2016) TUNTAP interfaces

(module tun
    (make-tun tun-name tun-fd tun-mac tun-path tun-up!)  

  (import chicken scheme)
  (use srfi-1 srfi-13 socket defstruct posix lolevel ioctl extras)
  
  (define IFREQ-SIZE 32)
  (define NAME-SIZE 16)

  (define SIOCGIFHWADDR #x8927)
  (define SIOCGIFFLAGS  #x8913)
  (define SIOCSIFFLAGS #x8914)

  (define TUNSETIFF #x400454ca)
  
  (define IFF-UP 1)
  (define IFF-RUNNING 64)

  (define IFF-TUN 1)
  (define IFF-TAP 2)
  (define IFF-NO-PI #x1000)
  
  (define (set-ifreq-zero! p #!optional (start 0) (end IFREQ-SIZE))
    (for-each (lambda (n)
		(pointer-u8-set! (pointer+ p (+ start n))
				 0))
	      (iota (- end start))))

  (define (set-ifreq-name! p name)
    (map (lambda (n c)
	   (pointer-u8-set! (pointer+ p n)
			    (char->integer c)))
	 (iota (min NAME-SIZE
		    (string-length name)))
	 (string->list name)))

  (define (set-ifreq-flag! p flag)
    (pointer-u8-set! (pointer+ p 16)
		     (bitwise-ior (remainder flag 256)
				  (pointer-u8-ref (pointer+ p 16))))
    (pointer-u8-set! (pointer+ p 17)
		     (bitwise-ior (quotient flag 256)
				  (pointer-u8-ref (pointer+ p 17)))))
  
  (define (get-ifreq-mac p)
    (fold (lambda (n acc) (+ (* acc 256)
			     (pointer-u8-ref (pointer+ p (+ 18 n)))))
	  0
	  (iota 6)))

  (define (get-ifreq-name p)
    (list->string
     (map integer->char
	  (reverse
	   (let lp ((ls '()) (n 0))
	     (let ((i (pointer-u8-ref (pointer+ p n))))
	       (if (zero? i)
		   ls
		   (lp (cons i ls) (+ n 1)))))))))

  (define (interface-up! name)
    (let* ((s (socket af/inet sock/dgram))
	   (sfd (socket-fileno s))
	   (p (allocate IFREQ-SIZE)))
      (set-ifreq-zero! p IFREQ-SIZE)      
      (set-ifreq-name! p name)
      (ioctl sfd SIOCGIFFLAGS p)
      (set-ifreq-flag! p (bitwise-ior IFF-UP IFF-RUNNING))
      (if (< 0 (ioctl sfd SIOCSIFFLAGS p))
	  signal 'failed-to-if-up)))

  (define (interface-mac name)
    (let* ((p (allocate IFREQ-SIZE))
	   (s (socket af/inet sock/dgram))
	   (sfd (socket-fileno s)))
      (set-ifreq-zero! p)
      (set-ifreq-name! p name)
      (ioctl sfd SIOCGIFHWADDR p)
      (get-ifreq-mac p)))

  (define tun-path (make-parameter "/dev/net/tun"))  

  (defstruct tun name fd)

  (define _make-tun make-tun)
  
  (define (tun-mac tun)
    (interface-mac (tun-name tun)))
  
  (define (make-tun #!optional (mode 'tun) (pi #f))
    ;;; create a tun/tap interface in three steps
    ;;; 1. Open tun path - "/dev/net/tun"
    ;;; 2. Call ioctl with TUNSETIFF and mode (tun vs tap)
    ;;; 3. Call ioctl again to get interface name
    (let ((fd (file-open (tun-path) open/rdwr))
	  (p (allocate IFREQ-SIZE)))
      (set-ifreq-zero! p)
      (set-ifreq-name! p (if (eq? mode 'tap)
			       "tap%d"
			       "tun%d"))

      (if (eq? mode 'tap)
	  (set-ifreq-flag! p IFF-TAP)
	  (set-ifreq-flag! p IFF-TUN))

      (and (not pi)
	   (set-ifreq-flag! p IFF-NO-PI))

      (if (< 0 (ioctl fd TUNSETIFF p))
	  (signal 'failed-to-open-tun-device))

      (_make-tun fd: fd
		 name: (get-ifreq-name p))))

  (define (tun-up! tun)
    (interface-up! (tun-name tun))))
