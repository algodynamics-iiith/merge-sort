class Array():
    def __init__(self, a, lo, hi):
        self.arr = a
        self.lo=lo
        self.hi=hi
        self.i = lo
        

    def in_range(self):
        return (self.i < self.hi)
    

    def inc(self):
        print("inc: i = " + str(self.i))
        if (self.i < self.hi):
            self.i = self.i + 1
        else:
            raise NameError("index i= " + str(self.i) + " is already at the high boundary")

    def set(self, val):
        print("set: i=" + str(self.i))
        if (self.i < self.hi):
            self.arr[self.i] = val
            self.i = self.i + 1
        else: 
            raise NameError("index i=" + str(self.i) + " is already at the high boundary")
