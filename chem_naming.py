#!/usr/bin/env python3
# coding: utf-8


'''
chem_naming.py

CHEMICAL NAMING

Name a chemical given its formula.

'''


import re


ELEMENT_PAT = r'([A-Z][a-z]?)([0-9]*)'
ELEMENT_RE = re.compile(ELEMENT_PAT)
VALID_FORMULA_RE = re.compile(r'(([A-Z][a-z]?)([0-9]*))+')


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
		
		
	def __str__(self):
		return self.symbol
		
		
	def __repr__(self):
		return self.symbol


# Elements incomplete!
ELEMENTS = {t[0]: Ion(*t) for t in (
	('H', 'hydrogen', +1),
	('Li', 'lithium', +1),
	('Be', 'beryllium', +2),
	('B', 'boron', ()),
	('C', 'carbon', -4, 'carb'),
	('N', 'nitrogen', -3, 'nitr'),
	('O', 'oxygen', -2, 'ox'),
	('F', 'fluorine', -1, 'fluor'),
	('Na', 'sodium', +1),
	('Mg', 'magnesium', +2),
	('Al', 'aluminum', +3),
	('Si', 'silicon', +3),
	('P', 'phosphorus', -3, 'phosph'),
	('S', 'sulfur', -2, 'sulf'),
	('Cl', 'chlorine', -1, 'chlor'),
	('Ag', 'silver', +1),
	('Zn', 'zinc', +2),
	('Cd', 'cadmium', +2),
	('Pb', 'lead', (+2, +4)),
	('Fe', 'iron', (+2, +3)),
	('Sn', 'tin', (+2, +4)),
	('Cr', 'chromium', (+2, +3, +6)),
	('Mn', 'manganese', (+2, +4, +7)),
	('Hg', 'mercury', (+1, +2)),
	('Cu', 'copper', (+1, +2)),
	('Ni', 'nickel', (+2, +3)),
	('Co', 'cobalt', (+2, +3))
)}


POLY_IONS = {i[0]: Ion(*i, is_compound=True) for i in (
	('H3O', 'hydronium', +1),
	('NH4', 'ammonium', +1),
	('OH', 'hydroxide', -1),
	('NO3', 'nitrate', -1),
	('NO2', 'nitrite', -1),
	('CO3', 'carbonate', -2),
	('CrO4', 'chromate', -2),
	('Cr2O7', 'dichromate', -2),
	('SCN', 'thiocyanate', -1),
	('S2O3', 'thiosulfate', -2),
	('SO4', 'sulfate', -2),
	('SO3', 'sulfite', -2),
	('CN', 'cyanide', -1),
	('ClO4', 'perchlorate', -1),
	('ClO3', 'chlorate', -1),
	('ClO2', 'chlorite', -1),
	('ClO1', 'hypochlorite', -1),
	('PO4', 'phosphate', -3),
	('PO3', 'phosphite', -3),
	('CH3COO', 'acetate', -1),
	('OH', 'hydroxide', -1),
	('MnO4', 'permanganate', -1),
	('SiO3', 'silicate', -2),
	('IO3', 'iodate', -1)
)}


IONS = {**ELEMENTS, **POLY_IONS}


def read_element(string: str, index: int = 0, convert: bool = True) -> (Ion, int, int):
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


def read_elements(string: str, convert: bool = True):
	index = 0
	elements = []
	while True:
		sym, num, index = read_element(string, index, convert)
		if sym:
			elements.append((sym, num))
		else:
			break
	return elements


def read_ion(string: str, index: int = 0) -> (Ion, int):
	# Try to read the biggest ion possible
	size = 0
	best_ion = None
	for ion_symbol in IONS:
		ion = IONS[ion_symbol]
		check_size = len(ion_symbol)
		check_string = string[index:index + check_size]
		if check_string == ion_symbol:
			if check_size > size:
				size = check_size
				best_ion = ion
	if best_ion.is_compound:
		end_index = index + size
	else:
		# It is an element, and might have a number after it...
		_, _, end_index = read_element(string, index)
	return (best_ion, end_index)
	

def roman(num: int) -> str:
	# We just need a few small integers because nothing should get bigger than 10
	if 1 <= num <= 10:
		return ('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x')[num - 1]
		

def validate_formula(formula):
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
	return True, None


def get_num_prefix(num):
	return PREFIXES[num]
	

def add_prefix(ion: Ion, amount: int) -> str:
	prefix = get_num_prefix(amount)
	return prefix + ion.name
	
	
def add_prefix_and_ide(ion: Ion, amount: int) -> str:
	prefix = get_num_prefix(amount)
	return prefix + ion.ide_name
	

def name_formula(formula: str) -> str:
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
		stem = anion_name[:-3] # take out the last 3
		# ___ous acid or ___ic acid
		if anion.name.endswith('ate'):
			return '{}ous acid'.format(stem)
		else:
			return '{}ic acid'.format(stem)
	elif first.is_metal:
		# Metal-nonmetal naming
		# Multiple charges?
		cation = first
		anion, _ = read_ion(formula, next_index)
		anion_ide = anion.ide_name
		if len(cation.charges) > 1:
			# Multiple charges
			number = -987654321 # TODO
			return '{} ({}) {}'.format(cation.name, number, anion_ide)
		return '{} {}'.format(cation.name, anion_ide)
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
		
		
def main():
	#print_all_ions()
	while True:
		f = input('Enter formula: ')
		if f:
			name = name_formula(f)
			print('-->',name,'\n')
		else:
			break
	
	
if __name__ == '__main__':
	main()

