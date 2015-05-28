import java.util.Scanner;
import java.util.Vector;

class Solver implements Runnable{
	int li, hi, lj, hj, Y, id;
	Solver(int li, int hi, int lj , int hj, int Y, int id){
		this.li = li; this.hi = hi; this.lj = lj; this.hj = hj;
		this.Y = Y; this.id = id;
	}
	@Override
	public void run() {
		for(int i = li; i <= hi; i++){
			for(int j = lj; j <= hj; j++){
				Trabalho.R[i][j]  = 0;
				for(int k = 0; k<= Y; k++){
					Trabalho.R[i][j] += Trabalho.A[i][k]*Trabalho.B[k][j];
				}
			}
		}
		System.out.println("[Thread " + id +"]" + " Terminou seu trabalho");
	}
}


public class Trabalho {

/*
Testes sem OUTPUT da matriz e elementos com valores aleatórios.
a partir de X >=1000 diminui o valor das outras dimensões
X = 10  
N =      1            2            3            4            8            16
Tempo:  0m0.102s      0m0.112s   0m0.096s     0m0.088s    0m0.092s       X
 
X = 100 , Y = 100, Z = 100
N =      1            2            3            4            8            16
Tempo:  0m0.402s    0m0.466s   0m0.467s      0m0.381s   0m0.410s       0m0.415s

X = 1000, Y = 1000, Z = 1000
N =      1            2            3            4            8            16
Tempo:  0m10.726s    0m16.772s   0m22.662s      0m24.770s  0m31.040s  0m21.029s

X = 10000, Y = 100, Z = 100
N =      1            2            3            4            8            16
Tempo:  0m1.533s      0m1.495s   0m1.582s      0m1.668s   0m1.552s       0m1.634s

X = 100000
N =      1            2            3            4            8            16
Tempo:  0m7.672s     0m9.106s   0m8.680s      0m9.294s   0m9.040s       0m9.023s
*/

	static int[][] A, B, R;
	
	static void pf(int[][] mat){

		//System.out.println("[Print Matrix]:");
		for(int i = 0; i < mat.length; i++){
			for(int j = 0; j < mat[i].length; j++) System.out.print(mat[i][j] + " ");
			System.out.println("");
		}
	}

	public static void main(String[] args) {
		Scanner in = new Scanner(System.in);
		Vector<Thread> v = new Vector<Thread>();
		int X, Y, Z, N;		
		N = in.nextInt();
		X = in.nextInt(); Y = in.nextInt(); Z = in.nextInt();
		
		Trabalho.A = new int[X][Y];
		Trabalho.B = new int[Y][Z];
		Trabalho.R = new int[X][Z];		
		
		for(int i = 0; i < X; i++) for(int j = 0; j < Y; j++) Trabalho.A[i][j] = in.nextInt();
		for(int i = 0; i < Y; i++) for(int j = 0; j < Z; j++) Trabalho.B[i][j] = in.nextInt();
		int QT = (X+N-1)/N; // 10/3 == 3

		for(int i = 0, j = 0; i < Math.min(N,X); i++, j+=QT) {
			v.add(new Thread(new Solver(j, Math.min(j+QT-1, X-1), 0, Z-1, Y-1, i) ) );
			System.out.println("Thread ["+i+"] " + " De " + j  + " ate " + Math.min(j+QT-1,X-1));
			v.lastElement().start();
		}
		try {
			for (int i = 0; i < v.size(); i++) v.get(i).join();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		System.out.println("\nResultado:");
		pf(Trabalho.R);
	}
}

