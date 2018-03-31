public class Test {

	public static int funLTR(int[] k){
		k[0] += 4;
		
		return 3 * k[0] - 1;
	}

	public static int funRTL(int[] k){
		k[0] += 4;

		return 3 * (k[0] - 1);
	}

	public static void main(String[] args) {
		System.out.println("Left to right order");
		int[] i = new int[] { 10 };
		int[] j = new int[] { 10 };
		int sum1, sum2;
		sum1 = (i[0] / 2) + funLTR(i);
		sum2 = funLTR(j) + (j[0] / 2);	

		System.out.println("sum1 = " + sum1);
		System.out.println("sum2 = " + sum2);

		System.out.println("Sumulate right to left order");
		i = new int[] { 10 };
		j = new int[] { 10 };
		sum1 = funRTL(i) + (i[0] / 2);
		sum2 = (j[0] / 2) + funRTL(j);

		System.out.println("sum1 = " + sum1);
		System.out.println("sum2 = " + sum2);

	}
}
