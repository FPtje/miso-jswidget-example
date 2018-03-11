<h1 align="center">Miso JS widget example</h1>
<p align="center">
  Elegantly embedding arbitrary Javascript widgets in Miso.
</p>

## Note:
🎉 Now working with the latest version of Miso! 🎉

Also currently incompatible with Miso's isomorphism feature.

## Try it live!
https://fptje.github.io/miso-jswidget-example/

## About

This example shows how to deeply integrate a javascript widget into your [Miso](https://haskell-miso.org/) app, such that:

- you can listen to custom events defined by the widget's library;
- the inevitable Javascript interop is encapsulated in a single component; and that
- this component can be re-used and placed anywhere in your application, following the [components](https://github.com/FPtje/miso-component-example) pattern;
- the state of your Miso `Model` is synced with the state of the widget, and the other way around.

This example embeds [flatpickr](https://flatpickr.js.org/), a date picker and calendar widget.

## The example explained
The example consists of two Haskell files: [`Main.hs`](src/Main.hs) and [`Flatpickr.hs`](src/Flatpickr.hs). `Main.hs` contains a pretty standard Miso app, with a `Model`, some `Action`s, `viewModel` function and is the parent of a `Flatpickr` component. `Main.hs` is not aware of any Javascript shenanigans.

`Flatpickr.hs` fully encapsulates the flatpickr widget. It takes care of the following:

- create the widget (using Javascript interop) when it should be visible, passing options given by its parent;
- create an [onChange](https://flatpickr.js.org/events/#onchange) event listener, turning all events into actions that are sent to its parent;
- define an [`Action`](https://github.com/FPtje/miso-jswidget-example/blob/master/src/Flatpickr.hs#L73-L75) with which the parent can modify the state of the widget, in our case just set the date;
- destroy the widget and cleans up any callbacks when the DOM element it was attached to disappears.

Comments in the code explain the details. Go check it out!

## Building the example
This example is built using [Nix](https://nixos.org/nix/).

Build by running the following command:
```bash
nix-build
```
Then open `result/index.html` to run the example.
