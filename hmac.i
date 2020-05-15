/*
 * hmac.i --
 *
 * Calculate an Hashed Message Authentication digest (HMAC) as described in
 * RFC 2104 (https://www.rfc-editor.org/rfc/rfc2104.txt).
 */

func hmac_sha1(key, data) { return hmac(sha1, 64, key, data); }
func hmac_md5(key, data) { return hmac(md5, 64, key, data); }
func hmac(hash, blk, key, data)
/* DOCUMENT hmac_sha1(key, data)
         or hmac_md5(key, data)
         of hmac(hash, blk, key, data)

      These functions calculate an Hashed Message Authentication digest (HMAC)
      of DATA combined with secret KEY and using SHA1, MD5 or a specified
      digest algorithm HASH.  HMACs are described in RFC 2104
      (https://www.rfc-editor.org/rfc/rfc2104.txt).

      If a hashing algorithm HASH is specified, BLK is the block size of the
      hashing algorithm and HASH should work as Yorick builtin functions md5
      and sha1.

      The function bytes2hex may be applied to the result to produce an
      hexadecimal string.

   SEE ALSO: md5, sha1, bytes2hex.
 */
{
    /* Check key. */
    typ = structof(key);
    if (typ == string && is_scalar(key)) {
        len = strlen(key);
    } else if (typ == char && is_vector(key)) {
        len = sizeof(key);
    } else if (! is_void(key)) {
        /* Void keys are allowed because this is equivalent to an empty
           string.  This is however not recommended of course. */
        error, "key must be a string, a vector of bytes or nothing";
    }

    /* Key is adjusted to be BLK bytes long.  If it is larger, then use the
       hash digest of key and pad this instead.  Note tha the final '\0' is
       not considered when directly hashing a string key, while the final '\0'
       is kept when converting string to an array of char. */
    if (len > blk) {
        key = hash(key);
        len = numberof(key);
    } else if (typ == string) {
        if (len > 0) {
            key = strchar(key);
            if (++len > blk) {
                key = key(1:blk);
            }
        } else {
            key = [];
        }
    }
    if (len < blk) {
        grow, key, array(char, blk - len);
    }

    /* Inner hash (`~` is Yorick XOR). */
    state = [];
    hash, state, key~char(0x36);
    data = hash(state, data);

    /* Outer hash (`~` is Yorick XOR). */
    state = [];
    hash, state, key~char(0x5c);
    return hash(state, data);
}

func bytes2hex(buf, up)
/* DOCUMENT bytes2hex(buf)
         or bytes2hex(buf, up)

      Convert a buffer BUF of bytes in an hexadecimal string.
      If optional argument UP is true, uppercase letters are used.

   SEE ALSO: hmac.
*/
{
    if (structof(buf) != char) {
        error, "expecting a buffer of bytes";
    }
    hex = strchar((up ? "0123456789ABCDEF" : "0123456789abcdef"));
    buf = (buf(*) & 0xff);
    tmp = array(char, 2, numberof(buf));
    tmp(1,) = hex((buf >> 4) + 1);
    tmp(2,) = hex((buf & 0x0f) + 1);
    buf = [];
    return strchar(tmp(*));
}

func hmac_tests
{
    nerrs = 0;

    key = array(char(0x0B), 16);
    msg = "Hi There";
    dig = bytes2hex(hmac_md5(key, msg));
    ans = "9294727a3638bb1c13f48ef8158bfc9d";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 1 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }
    dig = bytes2hex(hmac_sha1(key, msg), 1);
    ans = "675B0B3A1B4DDF4E124872DA6C2F632BFED957E9";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 2 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }

    key = "Jefe";
    msg = "what do ya want for nothing?";
    dig = bytes2hex(hmac_md5(key, msg));
    ans = "750c783e6ab0b503eaa86e310a5db738";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 2 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }
    dig = bytes2hex(hmac_sha1(key, msg));
    ans = "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 4 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }

    key = array(char(0xAA),16);
    msg = array(char(0xDD),50);
    dig = bytes2hex(hmac_md5(key, msg));
    ans = "56be34521d144c88dbb8c733f0e8b3f6";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 5 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }
    dig = bytes2hex(hmac_sha1(key, msg));
    ans = "d730594d167e35d5956fd8003d0db3d3f46dc7bb";
    if (dig != ans) {
        ++nerrs;
        write, format="Test 6 error - computed: %s\n", dig;
        write, format="               expected: %s\n", ans;
    }

    if (nerrs > 0) {
        error, "some tests failed";
    }
}
