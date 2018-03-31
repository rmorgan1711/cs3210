#include <stdio.h>  

int funLTR(int *k){
	(*k) += 4;

	return 3 * (*k) - 1;
}

int funRTL(int *k){
	(*k) += 4;

	return 3 * ((*k) - 1);
}

int main()  
{  
	printf("Left to right order\n");
	int i = 10, j = 10, sum1, sum2;
	sum1 = (i / 2) + funLTR(&i);
	sum2 = funLTR(&j) + (j / 2);

	printf("sum1 = %d\n", sum1);
	printf("sum2 = %d\n", sum2);

	printf("Sumulate right to left order\n");
	i = 10, j = 10;
	sum1 = funRTL(&i) + (i / 2);
	sum2 = (j / 2) + funRTL(&j);

	// sum1 = (i / 2) + (funRTL(&i));
	// sum2 = funRTL(&j) + ((j / 2));

	printf("sum1 = %d\n", sum1);
	printf("sum2 = %d\n", sum2);

    return 0;  
}   
