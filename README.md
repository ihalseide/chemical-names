# Project `chemical-names`

Project to take chemical formulas and convert them to the right chemical names, written in Common Lisp.

Examples:
- H2O = water
- O2 = dioxygen
- HCl = hydrochloric acid
- CO2 = carbon dioxide
- SF6 = sulfur hexafluoride
- HClO = hypochlorous acid

Github repository: https://github.com/ihalseide/chemical-names

## The Lisp package
The functions for this library are packaged in `com.div0.chemical-names`.
The exposed functions are:

##### name-formula `(name-formula formula)`
This takes a formula string and returns the resulting name as a string.
- Ex: `(name-formula "H2O") => "water"`

##### load-chem-data `(load-chem-data)`
This reloads the data in the text files with elements and compound data into the program for use by `name-formula`.

## Configurable files

The libary uses a few .txt files where the chemical information is stored (notice how there isn't one big file that just maps chemical formulas to names, the program actually DOES something).

- [compounds.txt]()
- [exceptions.txt]()
- [elements.txt]()
- [diatomic elements.txt]()

You should probably only mess with [compounds.txt]() and [exceptions.txt]() unless some new elements are discovered or something.
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