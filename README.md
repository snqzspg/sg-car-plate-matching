# SG Car Plate Matches
This application takes in a Singaporean car plate number with missing letters or numbers and prints out all possible numbers that matches the given pattern.
## Input
User will input the car plate number pattern 
## Example
Given a pattern of
```
SBA1234?
```
the application will give
```
Possible matches for "SBA1234?":
    SBA1234G
```
with the missing `G` filled.

Input 
```
SBA12??A
```
gives
```
Possible matches for "SBA12??A":
    SBA1210A
    SBA1228A
    SBA1245A
    SBA1262A
    SBA1297A
```
## Accuracy
Note that not all car plate numbers are considered, especially the more nuanced ones. Any feedback on inaccurate or missing car plate numbers are appreciated, although a pull request is much more greatly appreciated.

If you are using this for investigative purposes, you might want to cross check the results, or take into consideration that the result could be inaccurate.
## Building
### Prerequisites
Before you can build the project, you need to have the [Glasgow Haskell Compiler (GHC)](https://www.haskell.org/ghc/).
### Build
To build the project, you can run 
```
ghc possible_licence.hs
```
A `possible_licence` executable should be generated. You can use the `possible_license` directly. 

If you encounter issues with missing parts, you may need to link the parts dynamically with 
```
ghc -dynamic possible_license.hs
```
Alternatively, try to install static linking version of GHC.
## Background
Just like any other country, every vehicle in Singapore have a car plate number for identifications. However, unlike some countries, Singapore does not allow customisable number plates.

Car plates in Singapore follows the following format: `SBA1234G`

Breaking down into parts, we have:
- Prefix Letters (`SBA`)
- Numbers (`1234`)
- Checksum Letter (`G`)
Prefixes can be a single letter (Example `S`) or up to three letters (`SBA`). Private vehicles (cars) have `S_`, `E_`, `S__` and `T__` (in future), motorcycles have have `SA`, `A_`, `F_` and `F__`. Other types have specially assigned prefixes (Information is available on the [Wikipedia page](https://en.wikipedia.org/wiki/Vehicle_registration_plates_of_Singapore#Types_of_numbers))

Numbers are self explanatory.

The checksum letter is determined by an algorithm which takes in the rest of the letters and numbers and churn out a letter to be added at the back. This is useful to check the validity of a car plate without checking through the whole database.

The algorithm is detailed in the [Wikipedia page](https://en.wikipedia.org/wiki/Vehicle_registration_plates_of_Singapore#Checksum). However, there is not a lot of articles available describing the algorithm and one other article gave a different algorithm.

In this implementation I used the one on Wikipedia, which seems accurate so far.
## License
The algorithm behind the checksum may be copyrighted and thus an open-source license is not possible here. 
