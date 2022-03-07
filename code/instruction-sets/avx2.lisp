(in-package #:sb-simd-avx2)

(define-instruction-set :avx2
  (:include :avx)
  (:test (avx2-supported-p))
  (:simd-packs
   ;; You may wonder why we define these SIMD packs twice both for AVX and
   ;; AVX2.  The reason is that each of these instruction sets provides its
   ;; own version of the constructor for integer SIMD packs.  And since the
   ;; constructor has the same name as the pack, AVX2 cannot inherit them
   ;; from AVX.  Luckily, defining SIMD records is cheap, so we can simply
   ;; define them again with a different name (or rather, with the same
   ;; name but in a different package).
   (u8.32  u8  256 #:simd-pack-256-int (#:int-avx2-reg))
   (u16.16 u16 256 #:simd-pack-256-int (#:int-avx2-reg))
   (u32.8  u32 256 #:simd-pack-256-int (#:int-avx2-reg))
   (u64.4  u64 256 #:simd-pack-256-int (#:int-avx2-reg))
   (s8.32  s8  256 #:simd-pack-256-int (#:int-avx2-reg))
   (s16.16 s16 256 #:simd-pack-256-int (#:int-avx2-reg))
   (s32.8  s32 256 #:simd-pack-256-int (#:int-avx2-reg))
   (s64.4  s64 256 #:simd-pack-256-int (#:int-avx2-reg)))
  (:simd-casts
   (u8.32 u8.32-broadcast)
   (u16.16 u16.16-broadcast)
   (u32.8 u32.8-broadcast)
   (u64.4 u64.4-broadcast)
   (s8.32 s8.32-broadcast)
   (s16.16 s16.16-broadcast)
   (s32.8 s32.8-broadcast)
   (s64.4 s64.4-broadcast))
  (:reinterpret-casts
   (u8.16! sb-simd-avx::u8.16!-from-u8  sb-simd-avx::u8.16!-from-p128 u8.16!-from-p256)
   (u16.8! sb-simd-avx::u16.8!-from-u16 sb-simd-avx::u16.8!-from-p128 u16.8!-from-p256)
   (u32.4! sb-simd-avx::u32.4!-from-u32 sb-simd-avx::u32.4!-from-p128 u32.4!-from-p256)
   (u64.2! sb-simd-avx::u64.2!-from-u64 sb-simd-avx::u64.2!-from-p128 u64.2!-from-p256)
   (s8.16! sb-simd-avx::s8.16!-from-s8  sb-simd-avx::s8.16!-from-p128 s8.16!-from-p256)
   (s16.8! sb-simd-avx::s16.8!-from-s16 sb-simd-avx::s16.8!-from-p128 s16.8!-from-p256)
   (s32.4! sb-simd-avx::s32.4!-from-s32 sb-simd-avx::s32.4!-from-p128 s32.4!-from-p256)
   (s64.2! sb-simd-avx::s64.2!-from-s64 sb-simd-avx::s64.2!-from-p128 s64.2!-from-p256))
  (:instructions
   ;; f64.4
   (f64.4-permute4x64     #:vpermpd       (f64.4) (f64.4 imm8)    :cost 1)
   (f64.4-reverse         nil             (f64.4) (f64.4)         :cost 2 :encoding :fake-vop)
   ;; u8.16
   (u8.16!-from-p256      #:vextracti128  (u8.16) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (u8.16-broadcast       nil             (u8.16) (u8)            :cost 1 :encoding :fake-vop)
   (u8.16-broadcastvec    #:vpbroadcastb  (u8.16) (u8.16)         :cost 1)
   ;; u16.8
   (u16.8!-from-p256      #:vextracti128  (u16.8) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (u16.8-broadcast       nil             (u16.16) (u16)          :cost 1 :encoding :fake-vop)
   (u16.8-broadcastvec    #:vpbroadcastw  (u16.16) (u16.8)        :cost 1)
   ;; u32.4
   (u32.4!-from-p256      #:vextracti128  (u32.4) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (u32.4-broadcast       nil             (u32.4) (u32)           :cost 1 :encoding :fake-vop)
   (u32.4-broadcastvec    #:vpbroadcastd  (u32.4) (u32.4)         :cost 1)
   (u32.4-shiftl          #:vpsllvd       (u32.4) (u32.4 u32.4)   :cost 1)
   (u32.4-shiftr          #:vpsrlvd       (u32.4) (u32.4 u32.4)   :cost 1)
   ;; u64.2
   (u64.2!-from-p256      #:vextracti128  (u64.2) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (u64.2-broadcast       nil             (u64.2) (u64)           :cost 1 :encoding :fake-vop)
   (u64.2-broadcastvec    #:vpbroadcastq  (u64.2) (u64.2)         :cost 1)
   (u64.2-shiftl          #:vpsllvd       (u64.2) (u64.2 u64.2)   :cost 1)
   (u64.2-shiftr          #:vpsrlvd       (u64.2) (u64.2 u64.2)   :cost 1)
   ;; s8.16
   (s8.16!-from-p256      #:vextracti128  (s8.16) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (s8.16-broadcast       nil             (s8.16) (s8)            :cost 1 :encoding :fake-vop)
   (s8.16-broadcastvec    #:vpbroadcastb  (s8.16) (s8.16)         :cost 1)
   ;; s16.8
   (s16.8!-from-p256      #:vextracti128  (s16.8) (p256)          :cost 1 :suffix '(1) :always-translatable nil)
   (s16.8-broadcast       nil             (s16.16) (s16)          :cost 1 :encoding :fake-vop)
   (s16.8-broadcastvec    #:vpbroadcastw  (s16.16) (s16.8)        :cost 1)
   ;; s32.4
   (s32.4!-from-p256      #:vextracti128  (s32.4) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (s32.4-broadcast       nil             (s32.4) (s32)           :cost 1 :encoding :fake-vop)
   (s32.4-broadcastvec    #:vpbroadcastd  (s32.4) (s32.4)         :cost 1)
   (s32.4-shiftl          #:vpsllvq       (s32.4) (s32.4 s32.4)   :cost 1)
   (s32.4-shiftr          #:vpsrlvq       (s32.4) (s32.4 s32.4)   :cost 1)
   ;; s64.2
   (s64.2!-from-p256      #:vextracti128  (s64.2) (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (s64.2-broadcast       nil             (s64.2) (s64)           :cost 1 :encoding :fake-vop)
   (s64.2-broadcastvec    #:vpbroadcastq  (s64.2) (s64.2)         :cost 1)
   (s64.2-shiftl          #:vpsllvq       (s64.2) (s64.2 s64.2)   :cost 1)
   (s64.2-shiftr          #:vpsrlvq       (s64.2) (s64.2 s64.2)   :cost 1)
   ;; u8.32
   (make-u8.32            nil             (u8.32) (u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8) :cost 1 :encoding :fake-vop)
   (u8.32-values          nil             (u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8 u8) (u8.32) :cost 1 :encoding :fake-vop)
   (u8.32-broadcast       nil             (u8.32) (u8)            :cost 1 :encoding :fake-vop)
   (u8.32-broadcastvec    #:vpbroadcastb  (u8.32) (u8.32)         :cost 1)
   (u8.32-blend           #:vpblendvb     (u8.32) (u8.32 u8.32 u8.32) :cost 1)
   (two-arg-u8.32-and     #:vpand         (u8.32) (u8.32 u8.32)   :cost 1 :associative t)
   (two-arg-u8.32-or      #:vpor          (u8.32) (u8.32 u8.32)   :cost 1 :associative t)
   (two-arg-u8.32-xor     #:vpxor         (u8.32) (u8.32 u8.32)   :cost 1 :associative t)
   (u8.32-andc1           #:vpandn        (u8.32) (u8.32 u8.32)   :cost 1)
   (u8.32-not             nil             (u8.32) (u8.32)         :cost 1 :encoding :fake-vop)
   (two-arg-u8.32-max     #:vpmaxub       (u8.32) (u8.32 u8.32)   :cost 2 :associative t)
   (two-arg-u8.32-min     #:vpminub       (u8.32) (u8.32 u8.32)   :cost 2 :associative t)
   (two-arg-u8.32+        #:vpaddb        (u8.32) (u8.32 u8.32)   :cost 2 :associative t)
   (two-arg-u8.32-        #:vpsubb        (u8.32) (u8.32 u8.32)   :cost 2)
   (two-arg-u8.32=        #:vpcmpeqb      (u8.32) (u8.32 u8.32)   :cost 1 :associative t)
   (two-arg-u8.32/=       nil             (u8.32) (u8.32 u8.32)   :cost 2 :associative t :encoding :fake-vop)
   (two-arg-u8.32>~       #:vpcmpgtb      (u8.32) (u8.32 u8.32)   :cost 1)
   (two-arg-u8.32>        nil             (u8.32) (u8.32 u8.32)   :cost 1 :encoding :fake-vop)
   (two-arg-u8.32<        nil             (u8.32) (u8.32 u8.32)   :cost 1 :encoding :fake-vop)
   (two-arg-u8.32>=       nil             (u8.32) (u8.32 u8.32)   :cost 2 :encoding :fake-vop)
   (two-arg-u8.32<=       nil             (u8.32) (u8.32 u8.32)   :cost 2 :encoding :fake-vop)
   (u8.32-avg             #:vpavgb        (u8.32) (u8.32 u8.32)   :cost 2)
   (u8.32-packus          #:vpackuswb     (u8.32) (u16.16 u16.16) :cost 2)
   (u8.32-unpackhi        #:vpunpckhbw    (u8.32) (u8.32 u8.32)   :cost 1)
   (u8.32-unpacklo        #:vpunpcklbw    (u8.32) (u8.32 u8.32)   :cost 1)
   (u8.32-movemask        #:vpmovmskb     (u32)   (u8.32)         :cost 1)
   (u8.32-shuffle         #:vpshufb       (u8.32) (u8.32 u8.32)   :cost 1)
   (u8.32-permute128      #:vperm2i128    (u8.32) (u8.32 u8.32 imm8) :cost 1)
   (u8.32-extract128      #:vextracti128  (u8.16) (u8.32 imm1) :cost 1)
   (u8.32-insert128       #:vinserti128   (u8.32) (u8.32 u8.16 imm8) :cost 1)
   ;; u16.16
   (make-u16.16           nil            (u16.16) (u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16) :cost 1 :encoding :fake-vop)
   (u16.16-values         nil            (u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16 u16) (u16.16) :cost 1 :encoding :fake-vop)
   (u16.16-broadcast      nil            (u16.16) (u16)           :cost 1 :encoding :fake-vop)
   (u16.16-broadcastvec   #:vpbroadcastw (u16.16) (u16.16)        :cost 1)
   (u16.16-blend          #:vpblendvb    (u16.16) (u16.16 u16.16 u16.16) :cost 1)
   (u16.16-from-u8.16     #:vpmovzxbw    (u16.16) (u8.16)         :cost 5)
   (two-arg-u16.16-and    #:vpand        (u16.16) (u16.16 u16.16) :cost 1 :associative t)
   (two-arg-u16.16-or     #:vpor         (u16.16) (u16.16 u16.16) :cost 1 :associative t)
   (two-arg-u16.16-xor    #:vpxor        (u16.16) (u16.16 u16.16) :cost 1 :associative t)
   (u16.16-andc1          #:vpandn       (u16.16) (u16.16 u16.16) :cost 1)
   (u16.16-not            nil            (u16.16) (u16.16)        :cost 1 :encoding :fake-vop)
   (two-arg-u16.16-max    #:vpmaxuw      (u16.16) (u16.16 u16.16) :cost 2 :associative t)
   (two-arg-u16.16-min    #:vpminuw      (u16.16) (u16.16 u16.16) :cost 2 :associative t)
   (two-arg-u16.16+       #:vpaddw       (u16.16) (u16.16 u16.16) :cost 2 :associative t)
   (two-arg-u16.16-       #:vpsubw       (u16.16) (u16.16 u16.16) :cost 2)
   (two-arg-u16.16=       #:vpcmpeqw     (u16.16) (u16.16 u16.16) :cost 1 :associative t)
   (two-arg-u16.16/=      nil            (u16.16) (u16.16 u16.16) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-u16.16>~      #:vpcmpgtw     (u16.16) (u16.16 u16.16) :cost 1)
   (two-arg-u16.16>       nil            (u16.16) (u16.16 u16.16) :cost 1 :encoding :fake-vop)
   (two-arg-u16.16<       nil            (u16.16) (u16.16 u16.16) :cost 1 :encoding :fake-vop)
   (two-arg-u16.16>=      nil            (u16.16) (u16.16 u16.16) :cost 2 :encoding :fake-vop)
   (two-arg-u16.16<=      nil            (u16.16) (u16.16 u16.16) :cost 2 :encoding :fake-vop)
   (u16.16-shiftl         #:vpsllw       (u16.16) (u16.16 u16.8)  :cost 2)
   (u16.16-shiftr         #:vpsrlw       (u16.16) (u16.16 u16.8)  :cost 2)
   (u16.16-avg            #:vpavgw       (u16.16) (u16.16 u16.16) :cost 1)
   (u16.16-packus         #:vpackusdw    (u16.16) (u32.8 u32.8)   :cost 1)
   (u16.16-unpackhi       #:vpunpckhwd   (u16.16) (u16.16 u16.16) :cost 1)
   (u16.16-unpacklo       #:vpunpcklwd   (u16.16) (u16.16 u16.16) :cost 1)
   (u16.16-movemask       nil            (u16)    (u16.16)        :cost 1 :encoding :fake-vop)
   (u16.16-shufflehi      #:vpshufhw     (u16.16) (u16.16 imm8)   :cost 1)
   (u16.16-shufflelo      #:vpshuflw     (u16.16) (u16.16 imm8)   :cost 1)
   (u16.16-extract128     #:vextracti128 (u16.8)  (u16.16 imm1)   :cost 1)
   (u16.16-insert128      #:vinserti128  (u16.16) (u16.16 u16.8 imm1) :cost 1)
   (u16.16-permute128     #:vperm2i128   (u16.16) (u16.16 u16.16 imm8) :cost 1)
   ;; u32.8
   (make-u32.8            nil            (u32.8) (u32 u32 u32 u32 u32 u32 u32 u32) :cost 1 :encoding :fake-vop)
   (u32.8-values          nil            (u32 u32 u32 u32 u32 u32 u32 u32) (u32.8) :cost 1 :encoding :fake-vop)
   (u32.8-broadcast       nil            (u32.8) (u32)         :cost 1 :encoding :fake-vop)
   (u32.8-broadcastvec    #:vpbroadcastd (u32.8) (u32.8)       :cost 1)
   (u32.8-blend           #:vpblendvb    (u32.8) (u32.8 u32.8 u32.8) :cost 1)
   (u32.8-from-u16.8      #:vpmovzxwd    (u32.8) (u16.8)       :cost 5)
   (u32.8-from-u8.16      #:vpmovzxbd    (u32.8) (u8.16)       :cost 5)
   (two-arg-u32.8-and     #:vpand        (u32.8) (u32.8 u32.8) :cost 1 :associative t)
   (two-arg-u32.8-or      #:vpor         (u32.8) (u32.8 u32.8) :cost 1 :associative t)
   (two-arg-u32.8-xor     #:vpxor        (u32.8) (u32.8 u32.8) :cost 1 :associative t)
   (u32.8-andc1           #:vpandn       (u32.8) (u32.8 u32.8) :cost 1)
   (u32.8-not             nil            (u32.8) (u32.8)       :cost 1 :encoding :fake-vop)
   (two-arg-u32.8-max     #:vpmaxud      (u32.8) (u32.8 u32.8) :cost 2 :associative t)
   (two-arg-u32.8-min     #:vpminud      (u32.8) (u32.8 u32.8) :cost 2 :associative t)
   (two-arg-u32.8+        #:vpaddd       (u32.8) (u32.8 u32.8) :cost 2 :associative t)
   (two-arg-u32.8-        #:vpsubd       (u32.8) (u32.8 u32.8) :cost 2)
   (two-arg-u32.8=        #:vpcmpeqd     (u32.8) (u32.8 u32.8) :cost 1 :associative t)
   (two-arg-u32.8/=       nil            (u32.8) (u32.8 u32.8) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-u32.8>~       #:vpcmpgtd     (u32.8) (u32.8 u32.8) :cost 1)
   (two-arg-u32.8>        nil            (u32.8) (u32.8 u32.8) :cost 1 :encoding :fake-vop)
   (two-arg-u32.8<        nil            (u32.8) (u32.8 u32.8) :cost 1 :encoding :fake-vop)
   (two-arg-u32.8>=       nil            (u32.8) (u32.8 u32.8) :cost 2 :encoding :fake-vop)
   (two-arg-u32.8<=       nil            (u32.8) (u32.8 u32.8) :cost 2 :encoding :fake-vop)
   (u32.8-shiftl          #:vpsllvd      (u32.8) (u32.8 u32.8) :cost 1)
   (u32.8-shiftr          #:vpsrlvd      (u32.8) (u32.8 u32.8) :cost 1)
   (u32.8-unpackhi        #:vpunpckhdq   (u32.8) (u32.8 u32.8) :cost 1)
   (u32.8-unpacklo        #:vpunpckldq   (u32.8) (u32.8 u32.8) :cost 1)
   (u32.8-movemask        #:vmovmskps    (u8)    (u32.8)       :cost 1)
   (u32.8-extract128      #:vextracti128 (u32.4) (u32.8 imm1)  :cost 1)
   (u32.8-insert128       #:vinserti128  (u32.8) (u32.8 u32.4 imm1) :cost 1)
   (u32.8-permute128      #:vperm2i128   (u32.8) (u32.8 u32.8 imm8) :cost 1)
   ;; u64.4
   (make-u64.4            nil            (u64.4) (u64 u64 u64 u64) :cost 1 :encoding :fake-vop)
   (u64.4-values          nil            (u64 u64 u64 u64) (u64.4) :cost 1 :encoding :fake-vop)
   (u64.4-broadcast       nil            (u64.4) (u64)         :cost 1 :encoding :fake-vop)
   (u64.4-broadcastvec    #:vpbroadcastq (u64.4) (u64.4)       :cost 1)
   (u64.4-blend           #:vpblendvb    (u64.4) (u64.4 u64.4 u64.4) :cost 1)
   (u64.4-from-u16.8      #:vpmovzxwq    (u64.4) (u16.8)       :cost 5)
   (u64.4-from-u32.4      #:vpmovzxdq    (u64.4) (u32.4)       :cost 5)
   (u64.4-from-u8.16      #:vpmovzxbq    (u64.4) (u8.16)       :cost 5)
   (two-arg-u64.4-and     #:vpand        (u64.4) (u64.4 u64.4) :cost 1 :associative t)
   (two-arg-u64.4-or      #:vpor         (u64.4) (u64.4 u64.4) :cost 1 :associative t)
   (two-arg-u64.4-xor     #:vpxor        (u64.4) (u64.4 u64.4) :cost 1 :associative t)
   (u64.4-andc1           #:vpandn       (u64.4) (u64.4 u64.4) :cost 1)
   (u64.4-not             nil            (u64.4) (u64.4)       :cost 1 :encoding :fake-vop)
   (two-arg-u64.4+        #:vpaddq       (u64.4) (u64.4 u64.4) :cost 2 :associative t)
   (two-arg-u64.4-        #:vpsubq       (u64.4) (u64.4 u64.4) :cost 2)
   (two-arg-u64.4-mul     #:vpmuludq     (u64.4) (u64.4 u64.4) :cost 1 :associative t)
   (two-arg-u64.4=        #:vpcmpeqq     (u64.4) (u64.4 u64.4) :cost 1 :associative t)
   (two-arg-u64.4/=       nil            (u64.4) (u64.4 u64.4) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-u64.4>~       #:vpcmpgtq     (u64.4) (u64.4 u64.4) :cost 1)
   (two-arg-u64.4>        nil            (u64.4) (u64.4 u64.4) :cost 1 :encoding :fake-vop)
   (two-arg-u64.4<        nil            (u64.4) (u64.4 u64.4) :cost 1 :encoding :fake-vop)
   (two-arg-u64.4>=       nil            (u64.4) (u64.4 u64.4) :cost 2 :encoding :fake-vop)
   (two-arg-u64.4<=       nil            (u64.4) (u64.4 u64.4) :cost 2 :encoding :fake-vop)
   (u64.4-shiftl          #:vpsllvq      (u64.4) (u64.4 u64.4) :cost 1)
   (u64.4-shiftr          #:vpsrlvq      (u64.4) (u64.4 u64.4) :cost 1)
   (u64.4-unpackhi        #:vpunpckhqdq  (u64.4) (u64.4 u64.4) :cost 1)
   (u64.4-unpacklo        #:vpunpcklqdq  (u64.4) (u64.4 u64.4) :cost 1)
   (u64.4-movemask        #:vmovmskpd    (u4)    (u64.4)       :cost 1)
   (u64.4-extract128      #:vextracti128 (u64.2) (u64.4 imm1)  :cost 1)
   (u64.4-insert128       #:vinserti128  (u64.4) (u64.4 u64.2 imm1) :cost 1)
   (u64.4-permute128      #:vperm2i128   (u64.4) (u64.4 u64.4 imm8) :cost 1)
   ;; s8.32
   (make-s8.32            nil            (s8.32) (s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8) :cost 1 :encoding :fake-vop)
   (s8.32-values          nil            (s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8 s8) (s8.32) :cost 1 :encoding :fake-vop)
   (s8.32-broadcast       nil            (s8.32) (s8)             :cost 1 :encoding :fake-vop)
   (s8.32-broadcastvec    #:vpbroadcastb (s8.32) (s8.32)          :cost 1)
   (s8.32-blend           #:vpblendvb    (s8.32) (s8.32 s8.32 u8.32) :cost 1)
   (two-arg-s8.32-and     #:vpand        (s8.32) (s8.32 s8.32)    :cost 1 :associative t)
   (two-arg-s8.32-or      #:vpor         (s8.32) (s8.32 s8.32)    :cost 1 :associative t)
   (two-arg-s8.32-xor     #:vpxor        (s8.32) (s8.32 s8.32)    :cost 1 :associative t)
   (s8.32-andc1           #:vpandn       (s8.32) (s8.32 s8.32)    :cost 1)
   (s8.32-not             nil            (s8.32) (s8.32)          :cost 1 :encoding :fake-vop)
   (two-arg-s8.32-max     #:vpmaxsb      (s8.32) (s8.32 s8.32)    :cost 2 :associative t)
   (two-arg-s8.32-min     #:vpminsb      (s8.32) (s8.32 s8.32)    :cost 2 :associative t)
   (two-arg-s8.32+        #:vpaddb       (s8.32) (s8.32 s8.32)    :cost 2 :associative t)
   (two-arg-s8.32-        #:vpsubb       (s8.32) (s8.32 s8.32)    :cost 2)
   (two-arg-s8.32=        #:vpcmpeqb     (u8.32) (s8.32 s8.32)    :cost 1 :associative t)
   (two-arg-s8.32/=       nil            (u8.32) (s8.32 s8.32)    :cost 2 :associative t :encoding :fake-vop)
   (two-arg-s8.32>        #:vpcmpgtb     (u8.32) (s8.32 s8.32)    :cost 1)
   (two-arg-s8.32<        nil            (u8.32) (s8.32 s8.32)    :cost 1 :encoding :fake-vop)
   (two-arg-s8.32>=       nil            (u8.32) (s8.32 s8.32)    :cost 2 :encoding :fake-vop)
   (two-arg-s8.32<=       nil            (u8.32) (s8.32 s8.32)    :cost 2 :encoding :fake-vop)
   (s8.32-abs             #:vpabsb       (s8.32) (s8.32)          :cost 2)
   (s8.32-packs           #:vpacksswb    (s8.32) (s16.16 s16.16)  :cost 2)
   (s8.32-unpackhi        #:vpunpckhbw   (s8.32) (s8.32 s8.32)    :cost 1)
   (s8.32-unpacklo        #:vpunpcklbw   (s8.32) (s8.32 s8.32)    :cost 1)
   (s8.32-movemask        #:vpmovmskb    (u32)   (s8.32)          :cost 1)
   (s8.32-shuffle         #:vpshufb      (s8.32) (s8.32 u8.32)    :cost 1)
   (s8.32-sign            #:vpsignb      (s8.32) (s8.32 s8.32)    :cost 1)
   (s8.32-broadcastvec    #:vpbroadcastb (s8.32) (s8.32)          :cost 1)
   (s8.32-extract128      #:vextracti128 (s8.16) (s8.32 imm1) :cost 1)
   (s8.32-insert128       #:vinserti128  (s8.32) (s8.32 s8.16 imm1) :cost 1)
   (s8.32-permute128      #:vperm2i128   (s8.32) (s8.32 s8.32 imm8) :cost 1)
   ;; s16.16
   (make-s16.16           nil            (s16.16) (s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16) :cost 1 :encoding :fake-vop)
   (s16.16-values         nil            (s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16 s16) (s16.16) :cost 1 :encoding :fake-vop)
   (s16.16-broadcast      nil            (s16.16) (s16)           :cost 1 :encoding :fake-vop)
   (s16.16-broadcastvec   #:vpbroadcastw (s16.16) (s16.16)        :cost 1)
   (s16.16-blend          #:vpblendvb    (s16.16) (s16.16 s16.16 u16.16) :cost 1)
   (s16.16-from-s8.16     #:vpmovsxbw    (s16.16) (s8.16)         :cost 5)
   (s16.16-from-u8.16     #:vpmovsxbw    (s16.16) (u8.16)         :cost 5)
   (two-arg-s16.16-and    #:vpand        (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16-or     #:vpor         (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16-xor    #:vpxor        (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (s16.16-andc1          #:vpandn       (s16.16) (s16.16 s16.16) :cost 1)
   (s16.16-not            nil            (s16.16) (s16.16)        :cost 1 :encoding :fake-vop)
   (two-arg-s16.16-max    #:vpmaxsw      (s16.16) (s16.16 s16.16) :cost 2 :associative t)
   (two-arg-s16.16-min    #:vpminsw      (s16.16) (s16.16 s16.16) :cost 2 :associative t)
   (two-arg-s16.16+       #:vpaddw       (s16.16) (s16.16 s16.16) :cost 2 :associative t)
   (two-arg-s16.16-       #:vpsubw       (s16.16) (s16.16 s16.16) :cost 2)
   (two-arg-s16.16-mulhi  #:vpmulhw      (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16-mullo  #:vpmullw      (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16-mulhrs #:vpmulhrsw    (s16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16=       #:vpcmpeqw     (u16.16) (s16.16 s16.16) :cost 1 :associative t)
   (two-arg-s16.16/=      nil            (u16.16) (s16.16 s16.16) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-s16.16>       #:vpcmpgtw     (u16.16) (s16.16 s16.16) :cost 1)
   (two-arg-s16.16<       nil            (u16.16) (s16.16 s16.16) :cost 1 :encoding :fake-vop)
   (two-arg-s16.16>=      nil            (u16.16) (s16.16 s16.16) :cost 2 :encoding :fake-vop)
   (two-arg-s16.16<=      nil            (u16.16) (s16.16 s16.16) :cost 2 :encoding :fake-vop)
   (s16.16-abs            #:vpabsw       (s16.16) (s16.16)        :cost 2)
   (s16.16-hadd           #:vphaddw      (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-hadds          #:vphaddsw     (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-madd           #:vpmaddwd     (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-maddubs        #:vpmaddubsw   (s16.16) (u8.32 s8.32)   :cost 2)
   (s16.16-hsub           #:vphsubw      (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-hsubs          #:vphsubsw     (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-packs          #:vpackssdw    (s16.16) (s32.8 s32.8)   :cost 1)
   (s16.16-unpackhi       #:vpunpckhwd   (s16.16) (s16.16 s16.16) :cost 1)
   (s16.16-unpacklo       #:vpunpcklwd   (s16.16) (s16.16 s16.16) :cost 1)
   (s16.16-movemask       nil            (u16)    (s16.16)        :cost 1 :encoding :fake-vop)
   (s16.16-shufflehi      #:vpshufhw     (s16.16) (s16.16 imm8)   :cost 1)
   (s16.16-shufflelo      #:vpshuflw     (s16.16) (s16.16 imm8)   :cost 1)
   (s16.16-shiftl         #:vpsllw       (s16.16) (s16.16 s16.8)  :cost 2)
   (s16.16-shiftr         #:vpsrlw       (s16.16) (s16.16 s16.8)  :cost 2)
   (s16.16-sign           #:vpsignw      (s16.16) (s16.16 s16.16) :cost 2)
   (s16.16-extract128     #:vextracti128 (s16.8) (s16.16 imm1)    :cost 1)
   (s16.16-insert128      #:vinserti128  (s16.16) (s16.16 s16.8 imm1) :cost 1)
   (s16.16-permute128     #:vperm2i128   (s16.16) (s16.16 s16.16 imm8) :cost 1)
   ;; s32.8
   (make-s32.8            nil            (s32.8) (s32 s32 s32 s32 s32 s32 s32 s32) :cost 1 :encoding :fake-vop)
   (s32.8-values          nil            (s32 s32 s32 s32 s32 s32 s32 s32) (s32.8) :cost 1 :encoding :fake-vop)
   (s32.8-broadcast       nil            (s32.8) (s32)         :cost 1 :encoding :fake-vop)
   (s32.8-broadcastvec    #:vpbroadcastd (s32.8) (s32.8)       :cost 1)
   (s32.8-blend           #:vpblendvb    (s32.8) (s32.8 s32.8 u32.8) :cost 1)
   (s32.8-from-s16.8      #:vpmovsxwd    (s32.8) (s16.8)       :cost 5)
   (s32.8-from-u16.8      #:vpmovsxwd    (s32.8) (u16.8)       :cost 5)
   (s32.8-from-s8.16      #:vpmovsxbd    (s32.8) (s8.16)       :cost 5)
   (s32.8-from-u8.16      #:vpmovsxbd    (s32.8) (u8.16)       :cost 5)
   (two-arg-s32.8-and     #:vpand        (s32.8) (s32.8 s32.8) :cost 1 :associative t)
   (two-arg-s32.8-or      #:vpor         (s32.8) (s32.8 s32.8) :cost 1 :associative t)
   (two-arg-s32.8-xor     #:vpxor        (s32.8) (s32.8 s32.8) :cost 1 :associative t)
   (s32.8-andc1           #:vpandn       (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-not             nil            (s32.8) (s32.8)       :cost 1 :encoding :fake-vop)
   (two-arg-s32.8-max     #:vpmaxsd      (s32.8) (s32.8 s32.8) :cost 2 :associative t)
   (two-arg-s32.8-min     #:vpminsd      (s32.8) (s32.8 s32.8) :cost 2 :associative t)
   (two-arg-s32.8+        #:vpaddd       (s32.8) (s32.8 s32.8) :cost 2 :associative t)
   (two-arg-s32.8-        #:vpsubd       (s32.8) (s32.8 s32.8) :cost 2)
   (two-arg-s32.8-mullo   #:vpmulld      (s32.8) (s32.8 s32.8) :cost 2 :associative t)
   (two-arg-s32.8=        #:vpcmpeqd     (u32.8) (s32.8 s32.8) :cost 1 :associative t)
   (two-arg-s32.8/=       nil            (u32.8) (s32.8 s32.8) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-s32.8>        #:vpcmpgtd     (u32.8) (s32.8 s32.8) :cost 1)
   (two-arg-s32.8<        nil            (u32.8) (s32.8 s32.8) :cost 1 :encoding :fake-vop)
   (two-arg-s32.8>=       nil            (u32.8) (s32.8 s32.8) :cost 2 :encoding :fake-vop)
   (two-arg-s32.8<=       nil            (u32.8) (s32.8 s32.8) :cost 2 :encoding :fake-vop)
   (s32.8-abs             #:vpabsd       (s32.8) (s32.8)       :cost 2)
   (s32.8-hadd            #:vphaddd      (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-hsub            #:vphsubd      (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-shiftl          #:vpsllvd      (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-shiftr          #:vpsrlvd      (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-unpackhi        #:vpunpckhdq   (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-unpacklo        #:vpunpckldq   (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-movemask        #:vmovmskps    (u8)    (s32.8)       :cost 1)
   (s32.8-sign            #:vpsignd      (s32.8) (s32.8 s32.8) :cost 1)
   (s32.8-extract128      #:vextracti128 (s32.4) (s32.8 imm1)  :cost 1)
   (s32.8-insert128       #:vinserti128  (s32.8) (s32.8 s32.4 imm1) :cost 1)
   (s32.8-permute128      #:vperm2i128   (s32.8) (s32.8 s32.8 imm8) :cost 1)
   ;; s64.4
   (make-s64.4            nil            (s64.4) (s64 s64 s64 s64) :cost 1 :encoding :fake-vop)
   (s64.4-values          nil            (s64 s64 s64 s64) (s64.4) :cost 1 :encoding :fake-vop)
   (s64.4-broadcast       nil            (s64.4) (s64)         :cost 1 :encoding :fake-vop)
   (s64.4-broadcastvec    #:vpbroadcastq (s64.4) (s64.4)       :cost 1)
   (s64.4-blend           #:vpblendvb    (s64.4) (s64.4 s64.4 u64.4) :cost 1)
   (s64.4-from-s16.8      #:vpmovsxwq    (s64.4) (s16.8)       :cost 5)
   (s64.4-from-u16.8      #:vpmovsxwq    (s64.4) (u16.8)       :cost 5)
   (s64.4-from-s32.4      #:vpmovsxdq    (s64.4) (s32.4)       :cost 5)
   (s64.4-from-u32.4      #:vpmovsxdq    (s64.4) (u32.4)       :cost 5)
   (s64.4-from-s8.16      #:vpmovsxbq    (s64.4) (s8.16)       :cost 5)
   (s64.4-from-u8.16      #:vpmovsxbq    (s64.4) (u8.16)       :cost 5)
   (two-arg-s64.4-and     #:vpand        (s64.4) (s64.4 s64.4) :cost 1 :associative t)
   (two-arg-s64.4-or      #:vpor         (s64.4) (s64.4 s64.4) :cost 1 :associative t)
   (two-arg-s64.4-xor     #:vpxor        (s64.4) (s64.4 s64.4) :cost 1 :associative t)
   (s64.4-andc1           #:vandnpd      (s64.4) (s64.4 s64.4) :cost 1)
   (s64.4-not             nil            (s64.4) (s64.4)       :cost 1 :encoding :fake-vop)
   (two-arg-s64.4+        #:vpaddq       (s64.4) (s64.4 s64.4) :cost 1 :associative t)
   (two-arg-s64.4-        #:vpsubq       (s64.4) (s64.4 s64.4) :cost 1)
   (two-arg-s64.4-mul     #:vpmuldq      (s64.4) (s64.4 s64.4) :cost 1 :associative t)
   (two-arg-s64.4=        #:vpcmpeqq     (u64.4) (s64.4 s64.4) :cost 1 :associative t)
   (two-arg-s64.4/=       nil            (u64.4) (s64.4 s64.4) :cost 2 :associative t :encoding :fake-vop)
   (two-arg-s64.4>        #:vpcmpgtq     (u64.4) (s64.4 s64.4) :cost 1)
   (two-arg-s64.4<        nil            (u64.4) (s64.4 s64.4) :cost 1 :encoding :fake-vop)
   (two-arg-s64.4>=       nil            (u64.4) (s64.4 s64.4) :cost 2 :encoding :fake-vop)
   (two-arg-s64.4<=       nil            (u64.4) (s64.4 s64.4) :cost 2 :encoding :fake-vop)
   (s64.4-shiftl          #:vpsllvq      (s64.4) (s64.4 s64.4) :cost 1)
   (s64.4-shiftr          #:vpsrlvq      (s64.4) (s64.4 s64.4) :cost 1)
   (s64.4-unpackhi        #:vpunpckhqdq  (s64.4) (s64.4 s64.4) :cost 1)
   (s64.4-unpacklo        #:vpunpcklqdq  (s64.4) (s64.4 s64.4) :cost 1)
   (s64.4-movemask        #:vmovmskpd    (u4)    (s64.4)       :cost 1)
   (s64.4-extract128      #:vextracti128 (s64.2) (s64.4 imm1)  :cost 1)
   (s64.4-insert128       #:vinserti128  (s64.4) (s64.4 s64.2 imm1) :cost 1)
   (s64.4-permute128      #:vperm2i128   (s64.4) (s64.4 s64.4 imm8) :cost 1))
  (:loads
   (f32.4-ntload  #:vmovntdqa f32.4  f32vec f32-array f32.4-non-temporal-aref f32.4-non-temporal-row-major-aref)
   (f64.2-ntload  #:vmovntdqa f64.2  f64vec f64-array f64.2-non-temporal-aref f64.2-non-temporal-row-major-aref)
   (f32.8-ntload  #:vmovntdqa f32.8  f32vec f32-array f32.8-non-temporal-aref f32.8-non-temporal-row-major-aref)
   (f64.4-ntload  #:vmovntdqa f64.4  f64vec f64-array f64.4-non-temporal-aref f64.4-non-temporal-row-major-aref)
   (u8.16-ntload  #:vmovntdqa u8.16   u8vec  u8-array u8.16-non-temporal-aref u8.16-non-temporal-row-major-aref)
   (u16.8-ntload  #:vmovntdqa u16.8  u16vec u16-array u16.8-non-temporal-aref u16.8-non-temporal-row-major-aref)
   (u32.4-ntload  #:vmovntdqa u32.4  u32vec u32-array u32.4-non-temporal-aref u32.4-non-temporal-row-major-aref)
   (u64.2-ntload  #:vmovntdqa u64.2  u64vec u64-array u64.2-non-temporal-aref u64.2-non-temporal-row-major-aref)
   (s8.16-ntload  #:vmovntdqa s8.16   s8vec  s8-array s8.16-non-temporal-aref s8.16-non-temporal-row-major-aref)
   (s16.8-ntload  #:vmovntdqa s16.8  s16vec s16-array s16.8-non-temporal-aref s16.8-non-temporal-row-major-aref)
   (s32.4-ntload  #:vmovntdqa s32.4  s32vec s32-array s32.4-non-temporal-aref s32.4-non-temporal-row-major-aref)
   (s64.2-ntload  #:vmovntdqa s64.2  s64vec s64-array s64.2-non-temporal-aref s64.2-non-temporal-row-major-aref)
   (u8.32-ntload  #:vmovntdqa u8.32   u8vec  u8-array u8.32-non-temporal-aref  u8.32-non-temporal-row-major-aref)
   (u16.16-ntload #:vmovntdqa u16.16 u16vec u16-array u16.16-non-temporal-aref u16.16-non-temporal-row-major-aref)
   (u32.8-ntload  #:vmovntdqa u32.8  u32vec u32-array u32.8-non-temporal-aref  u32.8-non-temporal-row-major-aref)
   (u64.4-ntload  #:vmovntdqa u64.4  u64vec u64-array u64.4-non-temporal-aref  u64.4-non-temporal-row-major-aref)
   (s8.32-ntload  #:vmovntdqa s8.32   s8vec  s8-array s8.32-non-temporal-aref  s8.32-non-temporal-row-major-aref)
   (s16.16-ntload #:vmovntdqa s16.16 s16vec s16-array s16.16-non-temporal-aref s16.16-non-temporal-row-major-aref)
   (s32.8-ntload  #:vmovntdqa s32.8  s32vec s32-array s32.8-non-temporal-aref  s32.8-non-temporal-row-major-aref)
   (s64.4-ntload  #:vmovntdqa s64.4  s64vec s64-array s64.4-non-temporal-aref  s64.4-non-temporal-row-major-aref))
  (:stores
   (f32.4-ntstore  #:vmovntps f32.4  f32vec f32-array f32.4-non-temporal-aref  f32.4-non-temporal-row-major-aref)
   (f64.2-ntstore  #:vmovntpd f64.2  f64vec f64-array f64.2-non-temporal-aref  f64.2-non-temporal-row-major-aref)
   (f32.8-ntstore  #:vmovntps f32.8  f32vec f32-array f32.8-non-temporal-aref  f32.8-non-temporal-row-major-aref)
   (f64.4-ntstore  #:vmovntpd f64.4  f64vec f64-array f64.4-non-temporal-aref  f64.4-non-temporal-row-major-aref)
   (u8.16-ntstore  #:vmovntdq u8.16   u8vec  u8-array u8.16-non-temporal-aref  u8.16-non-temporal-row-major-aref)
   (u16.8-ntstore  #:vmovntdq u16.8  u16vec u16-array u16.8-non-temporal-aref  u16.8-non-temporal-row-major-aref)
   (u32.4-ntstore  #:vmovntdq u32.4  u32vec u32-array u32.4-non-temporal-aref  u32.4-non-temporal-row-major-aref)
   (u64.2-ntstore  #:vmovntdq u64.2  u64vec u64-array u64.2-non-temporal-aref  u64.2-non-temporal-row-major-aref)
   (s8.16-ntstore  #:vmovntdq s8.16   s8vec  s8-array s8.16-non-temporal-aref  s8.16-non-temporal-row-major-aref)
   (s16.8-ntstore  #:vmovntdq s16.8  s16vec s16-array s16.8-non-temporal-aref  s16.8-non-temporal-row-major-aref)
   (s32.4-ntstore  #:vmovntdq s32.4  s32vec s32-array s32.4-non-temporal-aref  s32.4-non-temporal-row-major-aref)
   (s64.2-ntstore  #:vmovntdq s64.2  s64vec s64-array s64.2-non-temporal-aref  s64.2-non-temporal-row-major-aref)
   (u8.32-ntstore  #:vmovntdq u8.32   u8vec  u8-array u8.32-non-temporal-aref  u8.32-non-temporal-row-major-aref)
   (u16.16-ntstore #:vmovntdq u16.16 u16vec u16-array u16.16-non-temporal-aref u16.16-non-temporal-row-major-aref)
   (u32.8-ntstore  #:vmovntdq u32.8  u32vec u32-array u32.8-non-temporal-aref  u32.8-non-temporal-row-major-aref)
   (u64.4-ntstore  #:vmovntdq u64.4  u64vec u64-array u64.4-non-temporal-aref  u64.4-non-temporal-row-major-aref)
   (s8.32-ntstore  #:vmovntdq s8.32   s8vec  s8-array s8.32-non-temporal-aref  s8.32-non-temporal-row-major-aref)
   (s16.16-ntstore #:vmovntdq s16.16 s16vec s16-array s16.16-non-temporal-aref s16.16-non-temporal-row-major-aref)
   (s32.8-ntstore  #:vmovntdq s32.8  s32vec s32-array s32.8-non-temporal-aref  s32.8-non-temporal-row-major-aref)
   (s64.4-ntstore  #:vmovntdq s64.4  s64vec s64-array s64.4-non-temporal-aref  s64.4-non-temporal-row-major-aref))
  (:associatives
   (u8.32-and two-arg-u8.32-and +u8-true+)
   (u8.32-or  two-arg-u8.32-or  +u8-false+)
   (u8.32-xor two-arg-u8.32-xor +u8-false+)
   (u8.32-max two-arg-u8.32-max nil)
   (u8.32-min two-arg-u8.32-min nil)
   (u8.32+    two-arg-u8.32+ 0)
   (u16.16-and two-arg-u16.16-and +u16-true+)
   (u16.16-or  two-arg-u16.16-or  +u16-false+)
   (u16.16-xor two-arg-u16.16-xor +u16-false+)
   (u16.16-max two-arg-u16.16-max nil)
   (u16.16-min two-arg-u16.16-min nil)
   (u16.16+    two-arg-u16.16+ 0)
   (u32.8-and two-arg-u32.8-and +u32-true+)
   (u32.8-or  two-arg-u32.8-or  +u32-false+)
   (u32.8-xor two-arg-u32.8-xor +u32-false+)
   (u32.8-max two-arg-u32.8-max nil)
   (u32.8-min two-arg-u32.8-min nil)
   (u32.8+    two-arg-u32.8+ 0)
   (u64.4-and two-arg-u64.4-and +u64-true+)
   (u64.4-or  two-arg-u64.4-or  +u64-false+)
   (u64.4-xor two-arg-u64.4-xor +u64-false+)
   (u64.4+    two-arg-u64.4+ 0)
   (u64.4-mul two-arg-u64.4-mul 1)
   (s8.32-and two-arg-s8.32-and +s8-true+)
   (s8.32-or  two-arg-s8.32-or  +s8-false+)
   (s8.32-xor two-arg-s8.32-xor +s8-false+)
   (s8.32-max two-arg-s8.32-max nil)
   (s8.32-min two-arg-s8.32-min nil)
   (s8.32+    two-arg-s8.32+ 0)
   (s16.16-and two-arg-s16.16-and +s16-true+)
   (s16.16-or  two-arg-s16.16-or  +s16-false+)
   (s16.16-xor two-arg-s16.16-xor +s16-false+)
   (s16.16-max two-arg-s16.16-max nil)
   (s16.16-min two-arg-s16.16-min nil)
   (s16.16+    two-arg-s16.16+ 0)
   (s16.16-mulhi two-arg-s16.16-mulhi 1)
   (s16.16-mullo two-arg-s16.16-mullo 1)
   (s16.16-mulhrs two-arg-s16.16-mulhrs 1)
   (s32.8-and two-arg-s32.8-and +s32-true+)
   (s32.8-or  two-arg-s32.8-or  +s32-false+)
   (s32.8-xor two-arg-s32.8-xor +s32-false+)
   (s32.8-max two-arg-s32.8-max nil)
   (s32.8-min two-arg-s32.8-min nil)
   (s32.8+    two-arg-s32.8+ 0)
   (s32.8-mullo two-arg-s32.8-mullo 1)
   (s64.4-and two-arg-s64.4-and +s64-true+)
   (s64.4-or  two-arg-s64.4-or  +s64-false+)
   (s64.4-xor two-arg-s64.4-xor +s64-false+)
   (s64.4+    two-arg-s64.4+ 0)
   (s64.4-mul two-arg-s64.4-mul 1))
  (:comparisons
   (u8.32=  two-arg-u8.32=  u8.32-and +u8-true+)
   (u8.32<  two-arg-u8.32<  u8.32-and +u8-true+)
   (u8.32<= two-arg-u8.32<= u8.32-and +u8-true+)
   (u8.32>  two-arg-u8.32>  u8.32-and +u8-true+)
   (u8.32>= two-arg-u8.32>= u8.32-and +u8-true+)
   (u16.16=  two-arg-u16.16=  u16.16-and +u16-true+)
   (u16.16<  two-arg-u16.16<  u16.16-and +u16-true+)
   (u16.16<= two-arg-u16.16<= u16.16-and +u16-true+)
   (u16.16>  two-arg-u16.16>  u16.16-and +u16-true+)
   (u16.16>= two-arg-u16.16>= u16.16-and +u16-true+)
   (u32.8=  two-arg-u32.8=  u32.8-and +u32-true+)
   (u32.8<  two-arg-u32.8<  u32.8-and +u32-true+)
   (u32.8<= two-arg-u32.8<= u32.8-and +u32-true+)
   (u32.8>  two-arg-u32.8>  u32.8-and +u32-true+)
   (u32.8>= two-arg-u32.8>= u32.8-and +u32-true+)
   (u64.4=  two-arg-u64.4=  u64.4-and +u64-true+)
   (u64.4<  two-arg-u64.4<  u64.4-and +u64-true+)
   (u64.4<= two-arg-u64.4<= u64.4-and +u64-true+)
   (u64.4>  two-arg-u64.4>  u64.4-and +u64-true+)
   (u64.4>= two-arg-u64.4>= u64.4-and +u64-true+)
   (s8.32=  two-arg-s8.32=  u8.32-and +u8-true+)
   (s8.32<  two-arg-s8.32<  u8.32-and +u8-true+)
   (s8.32<= two-arg-s8.32<= u8.32-and +u8-true+)
   (s8.32>  two-arg-s8.32>  u8.32-and +u8-true+)
   (s8.32>= two-arg-s8.32>= u8.32-and +u8-true+)
   (s16.16=  two-arg-s16.16=  u16.16-and +u16-true+)
   (s16.16<  two-arg-s16.16<  u16.16-and +u16-true+)
   (s16.16<= two-arg-s16.16<= u16.16-and +u16-true+)
   (s16.16>  two-arg-s16.16>  u16.16-and +u16-true+)
   (s16.16>= two-arg-s16.16>= u16.16-and +u16-true+)
   (s32.8=  two-arg-s32.8=  u32.8-and +u32-true+)
   (s32.8<  two-arg-s32.8<  u32.8-and +u32-true+)
   (s32.8<= two-arg-s32.8<= u32.8-and +u32-true+)
   (s32.8>  two-arg-s32.8>  u32.8-and +u32-true+)
   (s32.8>= two-arg-s32.8>= u32.8-and +u32-true+)
   (s64.4=  two-arg-s64.4=  u64.4-and +u64-true+)
   (s64.4<  two-arg-s64.4<  u64.4-and +u64-true+)
   (s64.4<= two-arg-s64.4<= u64.4-and +u64-true+)
   (s64.4>  two-arg-s64.4>  u64.4-and +u64-true+)
   (s64.4>= two-arg-s64.4>= u64.4-and +u64-true+))
  (:ifs
   (u8.32-if u8.32-blend)
   (u16.16-if u16.16-blend)
   (u32.8-if u32.8-blend)
   (u64.4-if u64.4-blend)
   (s8.32-if s8.32-blend)
   (s16.16-if s16.16-blend)
   (s32.8-if s32.8-blend)
   (s64.4-if s64.4-blend))
  (:reducers
   (u8.32- two-arg-u8.32- 0)
   (u16.16- two-arg-u16.16- 0)
   (u32.8- two-arg-u32.8- 0)
   (u64.4- two-arg-u64.4- 0)
   (s8.32- two-arg-s8.32- 0)
   (s16.16- two-arg-s16.16- 0)
   (s32.8- two-arg-s32.8- 0)
   (s64.4- two-arg-s64.4- 0))
  (:unequals
   (u8.32/=  two-arg-u8.32/=  u8.32-and  +u8-true+)
   (u16.16/= two-arg-u16.16/= u16.16-and +u16-true+)
   (u32.8/=  two-arg-u32.8/=  u32.8-and  +u32-true+)
   (u64.4/=  two-arg-u64.4/=  u64.4-and  +u64-true+)
   (s8.32/=  two-arg-s8.32/=  u8.32-and  +u8-true+)
   (s16.16/= two-arg-s16.16/= u16.16-and +u16-true+)
   (s32.8/=  two-arg-s32.8/=  u32.8-and  +u32-true+)
   (s64.4/=  two-arg-s64.4/=  u64.4-and  +u64-true+)))
