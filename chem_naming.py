#!/usr/bin/env python3
# coding: utf-8

"""
chem_naming.py

CHEMICAL NAMING

Name a chemical given its formula.
"""

import csv
from dataclasses import dataclass

# Prefixes for something like "hexa-fluoride"
PREFIXES = {
    1: 'mono',
    2: 'di',
    3: 'tri',
    4: 'tetra',
    5: 'penta',
    6: 'hexa',
    7: 'septa',
    8: 'octa',
    9: 'nova',
    10: 'deca'
}


@dataclass()
class ElementData:
    atomic_number: int
    symbol: str
    name: str
    period: int
    group: int
    is_metal: bool


class Element:

    def __init__(self, symbol, count=1, charge=0):
        self.symbol = symbol
        self.count = count
        self.charge = charge


@dataclass()
class IonData:
    symbol: str
    name: str
    charges: list


def to_roman(num: int) -> str:
    """
    Convert a integer from 1-10 to its Roman numeral equivalent (lower case)
    """
    # We just need a few small integers because nothing should get bigger than 10
    if 1 <= num <= 10:
        return ('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')[num - 1]


def name_formula(formula: str) -> str:
    """
    Given a chemical formula, name it using chemical naming rules.
    """
    # Exceptions
    if formula == 'H2O':
        return 'water'
    return "I don't know."


def standardize_formula(formula: str) -> str:
    # TODO: implement actual standardizing
    return formula


def main():
    """
    Get the user to input a formula and print out the result
    """
    while True:
        formula = input('Enter formula: ')
        if formula:
            try:
                name = name_formula(formula)
                print('--> I think the name for that formula is... ', name)
            except ValueError as err:
                print('(X)', err)
            print()
            continue
        break


def load_element_data(file):
    elements = []
    with open(file) as file:
        reader = csv.reader(file, delimiter=',')
        for i, element_row in enumerate(reader):
            # skip the first entry because it's a header
            if i == 0:
                continue
            atomic_number, symbol, name, period, group, metal_num = element_row
            atomic_number = int(atomic_number)
            symbol = symbol.title().strip()
            name = name.lower().strip()
            period = int(period)
            group = int(period)
            is_metal = not bool(int(metal_num))
            elements.append(ElementData(atomic_number, symbol, name, period, group, is_metal))
    return elements


def load_formula_data(file):
    formulas = {}
    with open(file) as file:
        reader = csv.reader(file, delimiter=',')
        for row in reader:
            formula, name = row
            formula = standardize_formula(formula)
            formulas[formula] = name
    return formulas


def load_ions_data(file):
    ions = []
    with open(file) as file:
        reader = csv.reader(file, delimiter=',')
        for row in reader:
            symbol = row[0]
            name = row[1]
            charges = [] # TODO: load charges from csv
            ions.append(IonData(symbol, name, charges))
    return ions


IONS = load_ions_data('ions.txt')
ELEMENTS = load_element_data('elements.txt')
FORMULAS = load_formula_data('formulas.txt')

if __name__ == '__main__':
    main()
