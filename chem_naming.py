#!/usr/bin/env python3
# coding: utf-8


'''
chem_naming.py

CHEMICAL NAMING

Name a chemical given its formula.

'''


import re


# An element is a uppercase letter maybe followed by a lowercase letter and a number
ELEMENT_PAT = r'([A-Z][a-z]?)([0-9]*)'
ELEMENT_RE = re.compile(ELEMENT_PAT)

# A valid formula is a bunch of elements strung together with no whitespace
VALID_FORMULA_RE = re.compile(r'(([A-Z][a-z]?)([0-9]*))+')

# For splitting up CSV values (separation by semicolon and maybe a tab)
CSV_RE = re.compile(r';\s?')

# A dictionary that maps symbols to Ion objects
IONS = dict() # (it will be loaded later)

# A dictionary that maps just element symbols to Ion objects
ELEMETS = dict() # (it will be loaded later)

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
	10: 'deca' # ?
}


class Ion:

	def __init__(self, symbol: str, name: str, charges: tuple,
					name_stem: str = None, is_compound: bool = False):
		self.symbol = str(symbol)
		self.name = str(name).lower()
		# make sure charges is a tuple
		self.charges = charges
		if not isinstance(self.charges, tuple):
			if not isinstance(self.charges, list):
				self.charges = [self.charges]
			self.charges = tuple(self.charges)
		self.name_stem = str(name_stem).lower()
		self.is_compound = bool(is_compound)


	def __str__(self):
		return self.symbol


	def __repr__(self):
		return self.symbol


	@property
	def is_metal(self) -> bool:
		if self.is_compound:
			raise ValueError('see code')
		return self.charges[0] > 0


	@property
	def ic_name(self) -> str:
		if self.symbol == 'S':
			# Sulfur exception
			return 'sulfuric'
		return self.name_stem + 'ic'


	@property
	def ide_name(self) -> str:
		return self.name_stem + 'ide'


def ion_charge_string_to_int(charge_str: str) -> int:
	working_str = charge_str.strip()
	return int(working_str[-1] + working_str[:-1]) # flip the sign to the beginning
	

def int_list_str_to_tuple(list_str: str) -> tuple:
	'''
	Converts a string that represents a list of ints to a tuple of ints.
	'''
	# Take off surrounding brackets and split by commas
	str_list = list_str.strip().split(',')
	# print('in <',list_str,'> out <',str_list,'>')
	# Convert each str element into an int
	return [ion_charge_string_to_int(s) for s in str_list]


def line_to_ion(line: str) -> Ion:
	data = re.split(CSV_RE, line)
	symbol, name, charges, name_stem, is_compound = data
	charges = int_list_str_to_tuple(charges)
	is_compound = (is_compound.lower() == 'true') # Convert boolean string to bool
	if not name_stem:
		name_stem = None
	return Ion(symbol, name, charges, name_stem, is_compound)


def get_ions_data(filename: str = 'ions.txt'):
	'''
	Read a CSV file for ion data
	'''
	ions_data = {}
	with open(filename) as file:
		for line in file:
			line = line.strip()
			if line:
				new_ion = line_to_ion(line)
				ions_data[new_ion.symbol] = new_ion
	return ions_data


# Load the ions and elements here
IONS = get_ions_data()
ELEMENTS = { symbol: ion for (symbol, ion) in IONS.items() if not ion.is_compound }


def read_element(string: str, index: int = 0, convert: bool = True) -> (Ion, int, int):
	'''
	Read 1 element from the formula and return a tuple
		(`element`, the amount of the element, and the next index)
	The `convert` flag determines whether to convert the element symbol from the formula into an `Ion`
	instance.
	'''
	substr = string[index:]
	result = ELEMENT_RE.search(substr)
	if result:
		symbol, num = result.group(1), result.group(2)
		if num:
			num = int(num)
		else:
			num = 1
		end_index = result.end() + index
		if convert:
			element = IONS[symbol]
			return element, num, end_index
		else:
			return symbol, num, end_index
	else:
		return None, None, index


def read_elements(formula: str, convert: bool = True) -> list:
	'''
	Reads as many of the elements from a formula as it can
	The `convert` boolean determines if the found elements will be converted into `Ion` instances, or
	just left as strings as they appear in the `formula`.
	Returns a list of tuple pairs with the element and the amount of the element
	'''
	index = 0
	elements = []
	while True:
		sym, num, index = read_element(formula, index, convert)
		if sym:
			elements.append((sym, num))
		else:
			break
	return elements


def read_ion(formula: str, index: int = 0) -> (Ion, int):
	'''
	Try to read the biggest known ion possible from the formula string and return the index
	that comes right after the found ion.
	If it returns a tuple with None as the first element, something went wrong...
	'''
	# Try to read the biggest ion possible
	size = 0
	best_ion = None
	for ion_symbol in IONS:
		ion = IONS[ion_symbol]
		check_size = len(ion_symbol)
		check_string = formula[index:index + check_size]
		if check_string == ion_symbol:
			if check_size > size:
				size = check_size
				best_ion = ion
	if best_ion.is_compound:
		end_index = index + size
	else:
		# It is an element, and might have a number after it...
		_, _, end_index = read_element(formula, index)
	return (best_ion, end_index)


def roman(num: int) -> str:
	'''
	Convert a integer from 1-10 to its Roman numeral equivalent (lower case)
	'''
	# We just need a few small integers because nothing should get bigger than 10
	if 1 <= num <= 10:
		return ('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')[num - 1]


def validate_formula(formula: str) -> (bool, str):
	'''
	Validates the formula by its syntax, not by checking if it is chemically possible.
	'''
	# check the conventions
	if not VALID_FORMULA_RE.fullmatch(formula):
		return False, 'Invalid format or characters'
	# check the elements exist and the charges add up
	# WARNING: THE CHARGES ADDING UP MIGHT HAVE EXCEPTIONS I DON'T KNOW ABOUT
	elements = read_elements(formula, False)
#	 total_charge = 0
	for element, num in elements:
		# Check that the element is valid
		if element not in ELEMENTS:
			return False, 'Invalid elemental symbol: "{}"'.format(element)
#		 charge = symbols[element].charges[0]
#		 total_charge += charge * num
#	 if not total_charge == 0:
#		 return False, 'Charge doesn\'t balance'
	return True, 'No problem'


def get_num_prefix(num):
	'''
	Get the prefix for a certain number.
	E.G. [1] -> [mono-]
	'''
	return PREFIXES[num]


def add_prefix(ion: Ion, amount: int) -> str:
	'''
	Get the name of an ion with a certain prefix.
	'''
	prefix = get_num_prefix(amount)
	return prefix + ion.name


def add_prefix_and_ide(ion: Ion, amount: int) -> str:
	'''
	Get the name of an ion in its "ide" form with a certain prefix.
	'''
	prefix = get_num_prefix(amount)
	return prefix + ion.ide_name


def name_formula(formula: str) -> str:
	'''
	Given a chemical formula, name it.
	(Works for non-organic chemistry and simpler formulas)
	'''
	# Exceptions
	if formula == 'H2O':
		return 'water'
	# Validate the formula
	is_valid, problem = validate_formula(formula)
	if not is_valid:
		raise ValueError('Invalid formula: ' + problem)
	# Now find out what type of naming to use
	first, next_index = read_ion(formula, 0) # Read the first ion
	if first == IONS['H']:
		# Acid naming
		# Diatomic?
		elements = read_elements(formula)
		if len(elements) == 2:
			other = elements[1][0]
			# Validate that the second element is a non-metal
			if not other.charges[0] < 0:
				raise ValueError()
			# Put into template with '-ic' form
			return 'hydro{} acid'.format(other.ic_name)
		# -ate or -ite?
		anion, _ = read_ion(formula, next_index)
		anion_name = anion.name
		if anion.name_stem:
			stem = anion.name_stem
		else:
			stem = anion_name[:-3] # take out the last 3
		# ___ous acid or ___ic acid
		if anion.name.endswith('ate'):
			return '{}ic acid'.format(stem)
		else:
			return '{}ous acid'.format(stem)
	elif first.is_metal:
		# Metal-nonmetal naming
		# Multiple charges?
		cation = first
		if next_index < len(formula):
			# There actually can be an anion...
			anion, _ = read_ion(formula, next_index)
			anion_ide = anion.ide_name
			if len(cation.charges) > 1:
				# Multiple charges
				if anion.is_compound:
					# TODO: for now, just use 1, but an actual number could come after parantheses
					number = 1
				else:
					_, anion_amount, _ = read_element(formula, next_index)
					number = anion_amount # number of anion atoms (huge oversimplification)
				return '{} ({}) {}'.format(cation.name, roman(number).upper(), anion_ide)
			return '{} {}'.format(cation.name, anion_ide)
		else:
			# No anion in the formula, so just return the cation
			return cation.name
	else:
		# Nonmetal-nonmetal naming
		# Prefixes
		parts = []
		first = True
		elements = read_elements(formula)
		for ion, amount in elements:
			if first:
				# Never add a mono- prefix
				# Don't use the -ide ending
				if amount == 1:
					ion_name = ion.name
				else:
					ion_name = add_prefix(ion, amount)
				first = False
			else:
				ion_name = add_prefix_and_ide(ion, amount)
			parts.append(ion_name)
		return ' '.join(parts)


def print_all_ions():
	for i in IONS.values():
		print(str.ljust(i.symbol, 6), '|', i.name)

		
def conform_formula(formula: str) -> str:
	'''
	Standardize equivalent formulas down to 1 guaranteed formula.
	'''
	pass


def main():
	'''
	Get the user to input a formula and print out the result
	'''
	#print_all_ions()
	while True:
		formula = input('Enter formula: ')
		if formula:
			try:
				name = name_formula(formula)
				print('-->', name)
			except ValueError as err:
				print('(X)', err)
			print()
		else:
			break


if __name__ == '__main__':
	main()

