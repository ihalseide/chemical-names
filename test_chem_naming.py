from unittest import TestCase
from chem_naming import read_next_element, Element


class Test(TestCase):

    def test_builtin_int(self):
        self.assertEqual(int('1'), 1)
        self.assertEqual(int(' 1'), 1)

    def test_read_next_element(self):
        self.assertEqual('H', read_next_element('H')[0].symbol, 'Simple symbol H')
        self.assertEqual('H', read_next_element('H2O')[0].symbol, 'Simple symbol H with distractions')
        self.assertEqual('O', read_next_element('H2O', 1)[0].symbol, 'Reading symbol from middle of string')

        element = read_next_element('H2O')[0]
        self.assertEqual('H', element.symbol)
        self.assertEqual(2, element.count)
        self.assertEqual(0, element.charge)
