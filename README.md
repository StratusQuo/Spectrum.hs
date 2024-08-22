# Spectrum.hs 🎨
### _Colorful Terminal Output in Haskell (Work in Progress)_

![2024-08-22_11-00-37 (1)](https://github.com/user-attachments/assets/e4308a50-784e-4009-917f-af317b7a4450)


This library provides a simple way to add color and style to your Haskell terminal output. It's a port of the OCaml [Spectrum](https://ocaml.org/p/spectrum/0.6.0) library.

It's still under development and considered **experimental** at the moment, so expect breaking changes for the next few releases.
* That said, feel free to experiment with it and contribute _(I can use the extra hands)_!

## Key Features:

* **Full Color Support**: Support for 8-Bit, 256-Color, and TrueColor
* **Named Colors:** Easily apply colors using keywords like `red`, `green`, `blue`, etc. For example the following prints bold yellow text: 
```haskell
putStrLn $ yellow $ bold "Bold Yellow"
```
* **Styles:** Support for most ANSI text styles, such as `bold`, `italic`, `underline`, `blink`,`Inverse`, etc.
* **RGB and Hex Colors:** Support for specifying colors using RGB values and hexadecimal codes.
* **Background Colors:** Set background colors using functions like `bgRed`, `bgGreen`, etc.
  * **Note:** _Subject to change with upcoming releases -- see "[Known Issues](https://github.com/StratusQuo/Spectrum.hs/edit/experimentFour/README.md#known-issues-and-limitations)" below_ 
* **Automatic Color Detection:** Adapts to different terminal capabilities and CI environments.

## Installation:

You can install Spectrum.hs using either `cabal` or `stack`:

### Using Cabal:

1. Add the dependency to your `cabal` file:
   ```cabal
   dependencies:
     - Spectrum
   ```
2. Run `cabal install`

### Using Stack (Recommended):

1. Add the dependency to your `stack.yaml` file:
   ```yaml
   dependencies:
     - Spectrum
   ```
2. Run `stack build`

**Usage:**

Here's a basic example of using Spectrum:

```haskell
import Spectrum.Colors

main :: IO ()
main = do
  putStrLn $ red "This text is red!"
  putStrLn $ bold $ blue "This text is bold and blue!"
  putStrLn $ rgb 255 0 0 "This text is red using RGB!"
  putStrLn $ hex "#00FF00" "This text is green using hex!"
```

### Testing

You can run a quick sanity test by building the library using:

```
stack build
```

And then running Main.hs using:

```
stack exec spectrum-hs-exe
```

Ideally you should see a test page print out.

## Known Issues and Limitations:

* **Bugs:** The library is still a WIP, mostly cobbled together over a few 3AM nights -- so expect *alot* of bugs as I work out a few the issues.
  * **Foreground and Background:** There's not an *easy* way to set a separate foreground and background color on the same text -- Ideally in the next release this will be ironed out, as i'm writing a few functions to make this easier to do.
  * **Broken Hex and RGB Colors:** The parsing logic for converting HEX to RGB isn't working for some reason _(colors are **hard**, man)_ but I should have a fix for that soon.
* **Experimental Features:** Some features like ANSI Code `21` "Double Underline" _(You should be able to find it under _"ECMA-48 Select Graphic Rendition"_ [in the man pages](https://man7.org/linux/man-pages/man4/console_codes.4.html))_ are experimental and may not work as expected in all terminals.
  * Tested and working in Wezterm
  * Partially working in Kitty
  * Other terminals are untested at this time.
* **Performance:** Performance hasn't been tested thoroughly yet.
  * However I _would_ like this to be able to be able to perform well when styling massive amounts of cloud logs, so performance improvements are definitely on the roadmap.
* **Documentation:** I'll be sure to put out more documentation as I continue working on the library -- in the meantime, feel free to send me a message with any questions you have.

## Roadmap

* Fixes for Hex and RGB Rendering
* Additonal support for the pipe `▷` operator at some point -- so one can use _(for example)_ `putStrLn ▷ red ▷ bold "Bold Red Text"` if they wish.
* Additional support for the [original](https://ocaml.org/p/spectrum/0.6.0) Spectrum syntax
* Performance Improvements 

## Contributing:

Contributions are welcome! Please feel free to open issues or submit pull requests.

## License:

MIT

## Disclaimer:

This library is a work in progress and is provided **as-is**. Use at your own risk! _(aka **not** in production)_
