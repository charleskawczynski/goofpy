import os
import sys
from collections import OrderedDict
import collections
import GOOFPY_directory as GD
import fortran_module as FM
import funcs as func
import inspect

class generator:
	def __init__(self):
		self.module = OrderedDict()
		self.module_list = []
		self.used_modules = []
		self.base_files = []
		self.base_modules = []
		return

	def set_directories(self,main,PS):
		self.d = GD.GOOFPY_directory()
		GOOFPY_dir = os.path.dirname(os.path.abspath(__file__))
		target_root = os.getcwd()
		self.d.set_dir(GOOFPY_dir,target_root,main,PS)
		func.delete_entire_tree_safe(self.d.target_dir)
		func.make_path(self.d.target_dir)
		func.make_path(self.d.target_root+'bin'+self.d.PS)
		self.base_dir = self.d.GOOFPY_dir+'base'+self.d.PS
		print(self.base_dir)
		return self

	def set_default_real(self,default_real): self.default_real = default_real

	def add_base_files(self,base_files): self.base_files = self.base_files+base_files
	def add_base_modules(self,base_modules): self.base_modules = self.base_modules+base_modules

	def print(self): self.d.print()

	def add_module(self,module_name):
		self.module_list = self.module_list+[module_name]
		self.module[module_name] = FM.fortran_module()
		self.module[module_name].set_default_real(self.default_real)
		self.module[module_name].set_name(module_name)
		return

	def generate_code(self):
		N_tot = 0
		print(' ----------------------------- module_list ----------------------------- ')
		print('\n'.join(self.module_list))
		print(' ----------------------------------------------------------------------- ')
		duplicates = [item for item, count in collections.Counter(self.module_list).items() if count > 1]
		# if not is_empty(duplicates):
		PS = self.d.PS
		if len(duplicates)>0:
			raise ValueError('Error: Duplicate classes: '+','.join(duplicates))

		for key in self.module:
			func.make_path(self.d.target_dir + self.module[key].folder_name + PS)

		for key in self.module:
			lofl = self.module[key].contruct_fortran_module(self.module_list,self.base_modules)
			L = lofl
			path = self.d.target_dir + self.module[key].folder_name + PS + key+self.d.fext
			# print(path.replace(self.d.target_root,''))
			func.write_string_to_file(path,'\n'.join(L))
			N_tot = N_tot+len(L)
		N_tot = N_tot
		base_spaces = self.module[key].base_spaces
		module_list_temp = [self.module[key].folder_name+PS+self.module[key].name for key in self.module]

		func.make_dot_bat(self.d.target_root,self.d.GOOFPY_dir,self.d.target_dir,module_list_temp,self.base_dir,self.base_files,self.d.PS)
		func.make_dummy_main(self.d.target_dir+'main_dummy.f90',self.module_list,base_spaces)
		print('Number of lines generated (Total): ' + str(N_tot))
