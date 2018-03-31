#include <stdio.h>
#include <time.h>

#define LENGTH 1000000
#define NUM_CALLS 50

long SumByRef(int seq[], int size){
	int sum = 0;
	for (int i=0; i < size; i++)
		sum += seq[i];

	return sum;
}

long SumByVal(int seq[], int size){
	// simulate pass by value by copying array
	int copy[size];
	for (int i = 0; i < size; i++)
		copy[i] = seq[i];

	long sum = 0;
	for (int i = 0; i < size; i++)
		sum += copy[i];

	return sum;
}

int main(){

	int intArray[LENGTH];
	for (int i = 0; i < LENGTH; i++)
		intArray[i] = 1;

	clock_t start;
	clock_t end;
	long sum;
	double timeRef, timeVal;
	
	start = clock();
	for (int i = 0; i < NUM_CALLS; i++)
		sum = SumByRef(intArray, LENGTH);
	end = clock();

	timeRef = (double)(end - start) / CLOCKS_PER_SEC;
	printf("Time taken for %d calls to pass by reference version:\n", NUM_CALLS);
	printf("%f\n", timeRef);

	start = clock();
	for (int i = 0; i < NUM_CALLS; i++)
		sum = SumByVal(intArray, LENGTH);
	end = clock();

	timeVal = (double)(end - start) / CLOCKS_PER_SEC;
	printf("Time taken for %d calls to pass by value:\n", NUM_CALLS);
	printf("%f\n", timeVal);

	printf("Pass by value took %f times as long.\n", timeVal / timeRef);
}
