#define fixnum_mask  0b11
#define fixnum_tag   0b00
#define fixnum_shift 2

#define is_fixnum(f)     ((f & fixnum_mask) == fixnum_tag)
#define fixnum_to_int(f) (f >> fixnum_shift)

#define bool_mask  0b1111111
#define bool_tag   0b0011111
#define bool_shift 7

#define is_bool(b)     ((b & bool_mask) == bool_tag)
#define bool_to_int(b) ((b >> bool_shift) & 0b1)

#define nil_mask 0b11111111
#define nil_tag  0b00101111

#define is_nil(v) ((v & nil_mask) == nil_tag)

#define val long long