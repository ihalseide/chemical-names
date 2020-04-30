# Project chemical-names

This is the source code for a small library for naming chemicals given their formulas and vice-versa. Written in Common Lisp by Izak Halseide. See the LICENSE.txt file for the license.

Github repository: https://github.com/ihalseide/chemical-names

## The Lisp package
The functions for this library are packaged in `com.div0.chemical-names`.
The exposed functions are:

##### name->formula 
`(name->formula name)`
This takes a chemical name and generates the correct formula as a string.
Not implemented yet.

##### formula->name
`(formula->name formula)`
This takes a formula string and returns the resulting name as a string.
- Ex: `(formula->name "H2O") => "water"`

##### load-chem-data 
`(load-chem-data)`
This reloads the data in the text files with elements and compound data into the program for naming compounds.

See [packages.lisp](https://www.github.com/ihalseide/chemical-names/blob/master/packages.lisp) for more info.

## Dependencies
The `com.div0.macro-utils` package (https://www.github.com/ihalseide/macro-utils) is used for some macros in the source code.

## Data files

The libary uses a few .txt files where the chemical information is stored (notice how there isn't one big file that just maps chemical formulas to names, the program actually DOES something).

- [compounds.txt](https://github.com/ihalseide/chemical-names/blob/master/compounds.txt)
- [exceptions.txt](https://github.com/ihalseide/chemical-names/blob/master/exceptions.txt)
- [elements.txt](https://github.com/ihalseide/chemical-names/blob/master/elements.txt)
- [diatomic elements.txt](https://github.com/ihalseide/chemical-names/blob/master/diatomic%20elements.txt)

You should probably only mess with compounds.txt and exceptions.txt unless some new elements are discovered or something.

## Naming conventions used

For this project, I used 3 different naming conventions, besides naming elements on their own. These 3 conventions are metal-nonmetal naming, nonmetal-nonmetal naming, and acid naming.

1. Metal-nonmetal naming
    This naming convention names the cation and the remaining anions. Cations are simply named by their element name, unless they can take on multiple charges, in which case they will have the charge written in Roman numerals following the name. The anion, if there are only 2 ions in the compound total, will take the "-ide" ending, otherwise it is just named.
    For example:
    - Pb(NO3)2 = lead (II) nitrate
    - NaCl = sodium chloride
    - CaCl2 = calcium chloride
    - NaOH = sodium hydroxide

2. Acid naming
	Acid naming is used when the cation of an ionic compound is H+. 
	- Binary acids are named as "hydro___ic acid", where the blank is filled by the root name of the anion. 
	    For example: 
        - HCl = hydro**chlor**ic acid since "chlor" is the root for "chlorine"
    - Ternary acids are named as either "\_\_\_ic acid" or "\_\_\_ous acid" depending on whether the anion is an "ate" ion or an "ite" ion, each filled in with the root name of the anion.
        For example: 
        - H3PO4 = phosphor**ic** acid, because it has phosph**ate**.
        - HNO2 = nitr**ous** acid, because it has nitr**ite**.

3. Nonmetal-nonmetal naming
    This naming is used when pretty much all of the components of the molecule are non-metals. It works by naming the individual elements in order of increasing abundance in the formula, where the most abundant element gets the "-ide" ending. Each element is prefixed to indicate how many of each element is in the compound. Also, the "mono-" prefix is dropped for the least abundant element.
    For example:
    - CO2 = carbon dioxide (the "mono-" was dropped from "monocarbon")
    - CO = carbon monoxide
    - SF6 = sulfur hexafluoride
    - N2O = dinitrogen monoxide
    
    The first 10 prefixes are: mono-, di-, tri-, tetra-, penta-, hexa-, hepta-, octa-, nona-, deca-.

Although these naming rules can cover quite a few compounds, there are some exceptions... hence the [exceptions.txt] file. Take water for example: even though it starts with Hydrogen, it is not an acid, so it's not hydroxic acid. And you might have heard it called dihydrogen monoxide too, but the accepted chemical name for H2O is indeed just "water".