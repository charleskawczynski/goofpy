class GOOFPY_directory:
	def __init__(self):
		return
	def set_dir(self,GOOFPY_dir,target_root,main,PS):
		self.PS = PS
		self.GOOFPY_dir = GOOFPY_dir+PS
		self.target_root = target_root+PS
		self.target_dir = target_root+PS+'generated_code'+PS
		self.main = main
		self.fext = '.f90'
		return self

	def print_local(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS          = '+self.PS)
		print('GOOFPY_dir  = '+self.GOOFPY_dir)
		print('target_root = '+self.target_root)
		print('target_dir  = '+self.target_dir)
		print('main        = '+self.main)
		print('fext        = '+self.fext)
		print('--------------------------------------')

	def print(self):
		print('-------------------------------------- GOOFPY directory')
		print('PS          = '+self.PS)
		print('GOOFPY_dir  = '+self.GOOFPY_dir)
		print('target_root = '+self.target_root)
		print('target_dir  = '+self.target_dir)
		print('main        = '+self.main)
		print('fext        = '+self.fext)
		print('--------------------------------------')

