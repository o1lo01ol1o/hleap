Web Socket interface for Leap Motion controllers
================================================

This Haskell package contains functions for interfacing with Leap Motion controllers, <<https://www.leapmotion.com/product/desktop>>.  It is based on the WebSocket API <<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>> and inspired by <<https://bitbucket.org/turion/jedinight/>>.  It has been tested with Service Version 2.3.1+31549 of the Web Sockets server from Leap Motion.

Skeletal example
----------------

This simple example program prints all Leap Motion events:

```haskell
main :: IO ()
main =
  runWithHandler def [setFocused True, setGestures True] $ \event ->
    print (event :: Event Float)
```
