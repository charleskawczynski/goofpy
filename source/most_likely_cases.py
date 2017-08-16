if       self.object_type=='primitive' and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='primitive' and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='primitive' and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='primitive' and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='primitive' and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='object'    and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='object'    and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='object'    and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='object'    and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='object'    and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='procedure' and     self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='procedure' and not self.allocatable and     self.dimension>1 and     self.rank>1: pass
elif     self.object_type=='procedure' and     self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='procedure' and not self.allocatable and     self.dimension>1 and not self.rank>1: pass
elif     self.object_type=='procedure' and not self.allocatable and not self.dimension>1 and not self.rank>1: pass
else: raise NameError('Case not caught!')
