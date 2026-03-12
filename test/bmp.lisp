(in-package #:binstruct.test)

(defbinstruct bmp-file-header ()
  (nil (coerce "BM" 'simple-base-string) :type (simple-base-string 2))
  (file-size 0 :type (unsigned-byte 32))
  (nil 0 :type (unsigned-byte 16))
  (nil 0 :type (unsigned-byte 16))
  (data-offset 0 :type (unsigned-byte 32)))

(defbinstruct bmp-info-header ()
  (header-size 0 :type (unsigned-byte 32))
  (width 0 :type (signed-byte 32))
  (height 0 :type (signed-byte 32))
  (color-planes 0 :type (unsigned-byte 16))
  (bits-per-pixel 0 :type (unsigned-byte 16))
  (compression-method 0 :type (unsigned-byte 32))
  (image-size 0 :type (unsigned-byte 32))
  (horizontal-resolution 0 :type (signed-byte 32))
  (vertical-resolution 0 :type (signed-byte 32))
  (colors-in-palette 0 :type (unsigned-byte 32))
  (important-colors 0 :type (unsigned-byte 32)))

(defbinstruct bmp ()
  (file-header (make-bmp-file-header) :type bmp-file-header)
  (info-header (make-bmp-info-header) :type bmp-info-header)
  (pixel-data (make-array 0 :element-type '(unsigned-byte 8)) 
              :type (simple-array (unsigned-byte 8) 
                                  ((floor (the fixnum (* (bmp-info-header-width info-header)
                                                         (bmp-info-header-height info-header)
                                                         (bmp-info-header-bits-per-pixel info-header))) 
                                          8)))))

(define-test bmp :parent suite
  (is-parse-equal (bmp)
    (#(#x42 #x4D           ; Signature "BM"
       #x36 #x00 #x00 #x00 ; File size: 54 bytes
       #x00 #x00           ; Reserved1
       #x00 #x00           ; Reserved2
       #x36 #x00 #x00 #x00 ; Data offset: 54 bytes
       #x28 #x00 #x00 #x00 ; Header size: 40 bytes
       #x01 #x00 #x00 #x00 ; Width: 1 pixel
       #x01 #x00 #x00 #x00 ; Height: 1 pixel
       #x01 #x00           ; Color planes: 1
       #x18 #x00           ; Bits per pixel: 24
       #x00 #x00 #x00 #x00 ; Compression method: 0 (none)
       #x00 #x00 #x00 #x00 ; Image size: 0 (can be 0 for uncompressed)
       #x12 #x34 #x00 #x00 ; Horizontal resolution: 0x3412
       #x56 #x78 #x00 #x00 ; Vertical resolution: 0x7856
       #x00 #x00 #x00 #x00 ; Colors in palette: 0
       #x00 #x00 #x00 #x00 ; Important colors: 0
       #x00 #x00 #x00)     ; Pixel data (1 pixel, 24 bits = 3 bytes, padded to 4)
      (make-bmp 
       :file-header (make-bmp-file-header 
                     :file-size 54
                     :data-offset 54)
       :info-header (make-bmp-info-header 
                     :header-size 40
                     :width 1
                     :height 1
                     :color-planes 1
                     :bits-per-pixel 24
                     :compression-method 0
                     :image-size 0
                     :horizontal-resolution #x3412
                     :vertical-resolution #x7856
                     :colors-in-palette 0
                     :important-colors 0)
       :pixel-data (make-array 3 :element-type '(unsigned-byte 8) 
                                 :initial-contents '(0 0 0))))))
