# Mozzg_SDK

This is my own SDK for general purpose classes.

## SDK contains:
#### Common units
-   ***cuClasses.***  Unit contains general purpose classes for creating double linked lists. This classes are used as a base classes for HashTables.
-   ***cuConsts.***  Unit contains constants for SDK.
-   ***cuHashTables.***  Unit contains classes for HashTables and their descendants. Collisions are solved using chaining technique. By default chains should be statically defined in descendant classes as fields for speed puprose, but you can change that behaviour by using defines in ***SDK_common_defines.inc.***
-   ***cuSystem.***  Unit contains classes for working with system. Currently only contains high precision timer and class method for high precision sleep.
-   ***uCustomExceptions.***  Unit contains exception classes, that are used in SDK.
-   ***SDK_common_defines.inc.***  Include file, containing compiler directives and defines for SDK.
#### Data manipulation units
-   ***uStream.***  Unit contains classes, that manipulate data as a stream.
-   ***uXML.***  Unit contains classes for working with XML.

## Compiling
This SDK is compiled and tested only in Rad Studio 10.4.2.

## Install
Clone this repository and add all folders, contained in Source folder to library path in IDE.

## License
Mozzg_SDK is distributed under [zlib/libpng License](https://opensource.org/licenses/Zlib), see [LICENSE](LICENSE).
Mozzg_SDK is free for any use. You can use this SDK in your project without any restrictions. You can omit to mention that you use Mozzg_SDK, but it would be appreciated.
