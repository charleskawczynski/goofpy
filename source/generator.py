import os
import sys
from collections import OrderedDict
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
		return

	def set_directories(self,main,PS):
		self.d = GD.GOOFPY_directory()
		GOOFPY_dir = os.path.dirname(os.path.abspath(__file__))
		target_root = os.getcwd()
		self.d.set_dir(GOOFPY_dir,target_root,main,PS)
		self.set_class_list()
		func.delete_entire_tree_safe(self.d.target_dir)
		func.make_path(self.d.target_dir)
		func.make_path(self.d.target_root+'bin'+self.d.PS)
		self.base_dir = self.d.GOOFPY_dir+'base'+self.d.PS
		print(self.base_dir)
		return self

	def add_base_files(self,base_files): self.base_files = self.base_files+base_files

	def print(self): self.d.print()

	def set_class_list(self):
		print('Main file: '+self.d.main)
		temp = func.read_file_to_list(self.d.main)
		temp = [x for x in temp if 'm_name=' in x.replace(' ','')]
		temp = [x for x in temp if not x.replace(' ','').replace('\t','').startswith("#")] # Remove comments
		temp = [x.split('=')[1] for x in temp]
		temp = [x.replace('\'','').replace(' ','') for x in temp]
		class_list = temp
		self.class_list = class_list
		return

	def add_module(self,module_name):
		self.module_list = self.module_list+[module_name]
		self.module[module_name] = FM.fortran_module()
		self.module[module_name].set_name(module_name)
		self.module[module_name].set_name(module_name)
		return

	def generate_code(self):
		N_tot = 0
		print(' ----------------------------- class_list ----------------------------- ')
		print('\n'.join(self.class_list))
		print(' ---------------------------------------------------------------------- ')
		for key in self.module:
			lofl = self.module[key].contruct_fortran_module(self.class_list)
			L = lofl
			# L = [item for sublist in lofl for item in sublist]
			path = self.d.target_dir + self.d.PS+key+self.d.fext
			func.write_string_to_file(path,'\n'.join(L))
			N_tot = N_tot+len(L)
		N_tot = N_tot
		base_spaces = self.module[key].base_spaces

		func.make_dot_bat(self.d.target_root,self.d.GOOFPY_dir,self.d.target_dir,self.class_list,self.base_dir,self.base_files,self.d.PS)
		func.make_dummy_main(self.d.target_dir+'main_dummy.f90',self.class_list,base_spaces)
		print('Number of lines generated (Total): ' + str(N_tot))
