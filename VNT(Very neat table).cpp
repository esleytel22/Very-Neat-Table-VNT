#include<iostream>
#include<fstream>


template <class T> class SA;
template <class T> class SM;
template <class T> std::ostream& operator<<(std::ostream& os, SA<T> s);
template < class T > class SA {

private:
	int low, high;
	T *p;
public:
	SA() {
		low = 0;
		high = -1;
		p = NULL;
	}
	SA(int l, int h) {
		if ((h - l + 1) <= 0) {
			std::cout << "out of bound error" << std::endl;
			exit(1);
		}
		low = l;
		high = h;
		p = new T[h - l + 1];
	}
	SA(int i) {
		low = 0;
		high = i - 1;
		p = new T[i];
	}
	
	SA(const SA & s) {
		int size = s.high - s.low + 1;
		p = new T[size];
		for (int i = 0; i < size; i++)
			p[i] = s.p[i];
		low = s.low;
		high = s.high;
	}
	
	// destructor
	~SA() {
		delete[]p;
	}
	
	T & operator[](int i) {
		if (i < low || i > high) {
			std::cout << "index " << i << " out of range" << std::endl;
			exit(1);
		}
		return p[i - low];
	}
	
	SA & operator=(const SA & s) {
		if (this == &s)
			return *this;
		delete[]p;
		int size = s.high - s.low + 1;
		p = new T[size];
		for (int i = 0; i < size; i++)
			p[i] = s.p[i];
		low = s.low;
		high = s.high;
		return *this;
	}
	
	int* operator+(int i) {
		return &p[low + i];
	}

};

template <class T> std::ostream & operator<<(std::ostream & os, SA < T > s) {
	int size = s.high - s.low + 1;
	for (int i = 0; i < size; i++)
		os << s.p[i] << std::endl;
	return os;
}


template < class T > class SM {
private:
	int row_low;
	int row_high;
	int col_low;
	int col_high;
	SA < SA < T > > matrix;

public:
	SM(int rows, int cols) {
		if (rows <= 0 || cols <= 0) {
			std::cout << "Please enter a valid row and columns size" << std::endl;
			exit(1);
		}
		row_low = 0;
		row_high = rows - 1;
		col_low = 0;
		col_high = cols - 1;
		matrix = SA < SA < T > >(rows);
		for (int j = 0; j < rows; j++)
			matrix[j] = SA < T >(cols);
	}

	SM() { }

	SM(int row_min, int row_max, int col_min, int col_max) {
		if ((row_max - row_min + 1) <= 0) {
			std::cerr << "constructor error in Matrix bounds definition" << std::endl;
			exit(1);
		}
		row_low = row_min;
		row_high = row_max;
		col_low = col_min;
		col_high = col_max;
		matrix = SA < SA < T > >(row_min, row_max);
		for (int i = row_min; i <= (row_max); i++)
			matrix[i] = SA < T >(col_min, col_max);
	}

	SM(int square_size) {
		row_low = 0;
		row_high = square_size - 1;
		col_low = 0;
		col_high = square_size - 1;
		matrix = SA < SA < T > >(square_size);
		for (int j = 0; j < square_size; j++)
			matrix[j] = SA < T >(square_size);
	}
	
	//destructor
	~SM() {
	}

	SA < T > &operator[](int i) {
		if (i < row_low || i > row_high) {
			std::cout << "index " << i << " out of range in Matrix" << std::endl;
			exit(1);
		}
		return matrix[i];
	}
	
	//Matrix Multiplication
	SM < T > operator*(SM & s) {
		if ((col_high - col_low + 1) != (s.row_high - s.row_low + 1)) {
			return 0;
		}
		int rows = (row_high - row_low + 1);
		int cols = (s.col_high - s.col_low + 1);
		SM < int >result(rows, cols);
		for (int r = 0; r < rows; r++) {
			for (int c = 0; c < cols; c++) {
				result[r][c] = 0;
			}
		}
		for (int r = 0; r < rows; r++) {
			for (int c = 0; c < cols; c++) {
				for (int i = 0; i < (s.row_high - s.row_low + 1); i++) {
					result[r][c] += ((*this)[r + row_low][i + col_low]) * (s[i + s.row_low][c + s.col_low]);
				}
			}
		}
		return result;
	}

	int getRows() {
		return row_high - row_low + 1;
	}

	int getCols() {
		return col_high - col_low + 1;
	}

	SA < T >* operator+(int i) {
		return &matrix[i];
	}
	
};

void printMatrix(int **matrix, int rows, int columns)
{
	for (int i = 0; i < rows; i++) {
		for (int j = 0; j < columns; j++) {
			std::cout << matrix[i][j] << ' ';
		}
		std::cout << '\n';
	}
}


class VNT {
public:
	SM<int> vnt;
	int p;
	int x;
	int y;
	int temp_x;
	int temp_y;

public:
	VNT(int a, int b) {
		vnt = SM<int>(a, b);
		x = a - 1;
		y = b - 1;
		temp_x = 0;
		temp_y = 0;
		for (int i = 0; i < a; i++)
			for (int j = 0; j < b; j++)
				vnt[i][j] = 0;
	}

	
	friend std::ostream & operator << (std::ostream & os, VNT  &a) {
		for (int i = a.x; i != -1; i--) {
			for (int j = a.y; j != -1; j--) {
				if (a.vnt[i][j] != 0)
					os << a.vnt[i][j] << " ";

			}
			os << std::endl;
		}
		return os;

	} 
	
	
	//adds to a non full vnt
	void Add(int a) {
		if (temp_y < y) {
			temp_x++;
			if (temp_x > x)
			{
				std::cout << "full";
				return;
			}
			else {

				temp_y = 0;

				vnt[temp_x][temp_y] = a;


				for (int i = 0; i <= y; i++)
					for (int j = 0; j <= y; j++)
						if (vnt[temp_x][i] > vnt[temp_x][j])
							std::swap(vnt[temp_x][i], vnt[temp_x][j]);

				for (int i = 0; i <= x; i++)
					for (int j = 0; j <= x; j++)
						if (vnt[i][temp_y] > vnt[j][temp_y])
							std::swap(vnt[i][temp_y], vnt[j][temp_y]);
				temp_y++;
				return;
			}
		}
		vnt[temp_x][temp_y] = a;


		for (int i = 0; i <= y; i++)
			for (int j = 0; j <= y; j++)
				if (vnt[temp_x][i] > vnt[temp_x][j])
					std::swap(vnt[temp_x][i], vnt[temp_x][j]);

		for (int i = 0; i <= x; i++)
			for (int j = 0; j <= x; j++)
				if (vnt[i][temp_y] > vnt[j][temp_y])
					std::swap(vnt[i][temp_y], vnt[j][temp_y]);

		temp_y++;

	}

	int getMin() {
		for (int i = x; i != -1; i--)
			for (int j = y; j != -1; j--)
				if (vnt[i][j] != 0)
					return vnt[i][j];



	}

	static void sort(int k[], int size) {
		for (int i = 0; i < size; i++)
			for (int j = 0; j < size; j++)
				if (k[i] < k[j])
					std::swap(k[i], k[j]);

	}

	bool find(int h) {
		for (int i = 0; i <= x; i++)
			for (int j = 0; j <= y; j++)
				if (vnt[i][j] == h)
					return true;
		return false;

	}

};

int main() {
	std::ofstream myfile("matrix.txt");
	VNT A(5, 7);
	A.Add(25);
	A.Add(10);
	A.Add(2);
	A.Add(5);


	operator<<(myfile, A);
	//myfile << A;
	
	myfile << "getMin: " << A.getMin() << std::endl;
	myfile << "find: (7) "<<A.find(7) << std::endl;
	myfile << "find: (25) " << A.find(25) << std::endl;




}

