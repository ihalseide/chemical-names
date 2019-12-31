#!/usr/bin/env python3


import unittest


import chem_naming
from chem_naming import read_ion
from chem_naming import name_formula


class TestReadIon(unittest.TestCase):

	def test_ions(self):
		# Go through all the ions and make sure the function matches just that ion back
		#  to itself.
		for ion_symbol in chem_naming.IONS:
			result = read_ion(ion_symbol)[0]
			self.assertEqual(result, chem_naming.IONS[ion_symbol])
		
	def test_helped_salt(self):
		formula = 'NaCl'
		result, _ = read_ion(formula)
		self.assertEqual(result, chem_naming.IONS['Na'])
		result, _ = read_ion(formula, 2)
		self.assertEqual(result, chem_naming.IONS['Cl'])
		
		
	def test_salt(self):
		formula = 'NaCl'
		result, index = read_ion(formula)
		print(index)
		self.assertEqual(result, chem_naming.IONS['Na'])
		result, _ = read_ion(formula, index)
		self.assertEqual(result, chem_naming.IONS['Cl'])
		
		
	def test_this(self):
		result, index = read_ion('H2PO4')
		
		
class TestReadFormula(unittest.TestCase):
	def test_nonmetal(self):
		result = name_formula('H2O')
		self.assertEqual(result, 'water')
		
		result = name_formula('CO2')
		self.assertEqual(result, 'carbon dioxide')
		
		result = name_formula('O2')
		self.assertEqual(result, 'dioxygen')
		
		result = name_formula('SF6')
		self.assertEqual(result, 'sulfur hexafluoride')
		
	def test_acids(self):
		result = name_formula('HCl')
		self.assertEqual(result, 'hydrochloric acid')
		
		result = name_formula('HF')
		self.assertEqual(result, 'hydrofluoric acid')
		
		result = name_formula('H2CO3')
		self.assertEqual(result, 'carbonic acid')
		
	def test_metal_nonmetal(self):
		result = name_formula('NaCl')
		self.assertEqual(result, 'sodium chloride')
		
		
if __name__ == '__main__':
	unittest.main()