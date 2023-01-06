# motion-streamer

## Building from sources

There is no binary distribution yet, so you will need to download this repository and compile the sources.
I have not tested cross-compilation, so I currently install all dependencies in the RPi and compile there.

Also note that this was written based on my experience with the [Raspberry Pi OS](https://www.raspberrypi.com/software/) Bullseye, so in other operating systems your mileage may vary.

### Required packages to compile


```sh
apt install libgirepository1.0-dev libgstreamer1.0-dev llvm libnuma-dev gstreamer1.0-opencv
```

### Haskell compiler

While there is a package for `ghc` and `cabal-install` on Bullseye I had problems compiling under that version.
The software was tested using GHC 9.2.5 and cabal 3.6.2.0.
You can install from binary packages using [GHCup](https://www.haskell.org/ghcup/).
Once you install GHCup try the command `ghcup tui` for a neat terminal ui that will show you all compiler and tool versions available.

### Compile and run the application

```sh
cabal run motion-streamer
```

In case you see the error `motion-streamer: FactoryError` it is probably due to the lack of a `libcamerasrc` element in your gstreamer instalation.
To install libcamera from sources check their [getting started section](https://libcamera.org/getting-started.html)

You can still run without libcamera using a test pattern:

```sh
MS_TEST=true cabal run motion-streamer
```

You can also configure the host (defaults to "0.0.0.0") and port (defaults to 5000) for tcp video streaming:

```sh
MS_HOST=localhost MS_PORT=8080 cabal run motion-streamer
```

## Using the camera

Once you have the camera setup, you can run the application without the `MS_TEST` flag.
**Important** The application always starts recording regardless of any movement. So if you have a static scene, move in front of the camera so the motion sensor will stop recording.

### Streaming the video captured by the camera

Unless you use the `MS_HOST` configuration, the camera will be streaming via TCP to all network interfaces.
To watch the stream from any computer where `rpi` is a hostname that points to the RPi running motion-streamer use `gstreamer-launch-1.0` (provided by the package `gstreamer1.0-tools`):

```sh 
gst-launch-1.0 -v tcpclientsrc host=rpi port=5000 ! jpegdec ! decodebin ! videoconvert ! autovideosink
```

