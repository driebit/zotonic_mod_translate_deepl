# zotonic_mod_translate_deepl

Automatic translation of texts using DeepL

If this module is enabled then the _translate_ option will be added to the 
dialog shown when adding a language. This allows to translate texts from an
existing language to another language.

## API key configuration

There is a single configuration key:

 * `mod_translate_deepl.api_key` Set to the API key from DeepL.

If the API key ends with `:fx` then the free api endpoint at `https://api-free.deepl.com` is
used, otherwise the paid endpoint at `https://api.deepl.com` is used.

## ACL configurarion

You must also allow the user to use the DeepL integration. This is to prevent an open API endpoint
to access DeepL, using the paid account of the site. Add `use.mod_translate_deepl` for the
user groups that are allowed to use DeepL.

## Languages supported

DeepL does not support all possible language combinations.
Check here for the supported language combinations:

https://support.deepl.com/hc/en-us/articles/360019925219-Languages-included-in-DeepL-Pro
