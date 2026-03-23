library(Rcpp)

cppFunction('
  NumericVector bubbleSort(NumericVector x) {
    NumericVector arr = x;
    for (int i = 0; i < x.size() - 1; i++) {
        for (int j = 0; j < x.size() - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
    return arr;
}')

cppFunction('

NumericVector mergeSort(NumericVector x ,int l,int r) {
    NumericVector arr=x;
    if (l < r) {
        int m = l + (r - l) / 2;

        mergeSort(arr, l, m);
        mergeSort(arr, m + 1, r);

        int n1 = m - l + 1;
        int n2 = r - m;

        int L[n1], R[n2];
        for (int i = 0; i < n1; i++) {
            L[i] = arr[l + i];
        }
        for (int j = 0; j < n2; j++) {
            R[j] = arr[m + 1 + j];
        }

        int i = 0, j = 0, k = l;
        while (i < n1 && j < n2) {
            if (L[i] <= R[j]) {
                arr[k] = L[i];
                i++;
            } else {
                arr[k] = R[j];
                j++;
            }
            k++;
        }

        while (i < n1) {
            arr[k] = L[i];
            i++;
            k++;
        }

        while (j < n2) {
            arr[k] = R[j];
            j++;
            k++;
        }
    }
    return arr;
}
')

x<-c(343,5432,4,2323,5,53,334)
l<-0
r<-length(x)
mergeSort(x,l,r-1)


bubbleSort(x)


cppFunction('
int binarySearch(NumericVector z, int l, int r, int x) {
    NumericVector arr = z;
    if (r >= l) {
        int mid = l + (r - l) / 2;

        if (arr[mid] == x) {
            return mid;
        }
        else if (arr[mid] > x) {
            return binarySearch(arr, l, mid - 1, x);
        }
        else {
            return binarySearch(arr, mid + 1, r, x);
        }
    }
    return -1;
}
')

x<-c(343,5432,4,2323,5,53,334)
l<-0
r<-length(x)
x<-bubbleSort(x)
x
binarySearch(x,l,r-1,2323)

cppFunction('
int linearSearch(NumericVector x,int s) {
    NumericVector arr = x;
    int n = arr.size();
    for (int i = 0; i < n; i++) {
        if (arr[i] == s) {
            return i; // Return the index of the element if found
        }
    }
    return -1; // Return -1 if the element is not found in the array
}
')
x<-c(343,5432,4,2323,5,53,334)
linearSearch(x,53)

cppFunction('double sumSquares(NumericVector x) {
  int n = x.size();
  double sum = 0.0;
  
  for(int i = 0; i < n; i++) {
    sum += x[i] * x[i];
  }
  
  return sum;
}')

sumSquares(c(2,3,5,6,7))



