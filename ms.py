from arr import *

# sets dest to src and increments src
def shift(src,dest):
    dest.set(src.arr[src.i])
    src.inc()

# copies elements of d from p till len(d)
# into c
def shift_rest(src, dest):
    while (src.in_range()):
        shift(src, dest)


# shift(smaller(left, right), c)
def smaller(left,right):
    if (left.arr[left.i] <= right.arr[right.i]):
        return left
    else:
        return right


def shift_full(left, right, c):
    while (left.in_range() and right.in_range()) :
        shift(smaller(left, right), a))
    shift_rest(left, a)
    shift_rest(right, a)

def merge(a, l, m, h):
    left  = Array(a[l:m],0, m-l) # a[l:m] = a[l..m) is a copy
    right = Array(a[m:r],0, h-m) # a[m:r] = a[m..r) is a copy
    a     = Array(a, l, h)
    shift_full(left, right, a)

    
def mergeSort(arr,l,h):
	if l < h:
		m = (l+h)//2 # floor
		mergeSort(arr, l, m)
		mergeSort(arr, m, h)
		merge(arr, l, m, h)




