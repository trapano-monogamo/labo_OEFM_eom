#include <iostream>
#include <fstream>
#include <vector>

#include "TApplication.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TLegend.h"
#include "TAxis.h"

using namespace std;

struct Measure
{
	double x, ex, y, ey;

	friend istream &operator>>(istream &in, Measure &m)
	{
		in >> m.x >> m.y >> m.ex >> m.ey;
		return in;
	}
	friend ostream &operator<<(ostream &out, Measure &m)
	{
		out << "{" << m.x << " +- " << m.ex
			<< ", " << m.y << " +- " << m.ey << "}";
		return out;
	}
};

vector<Measure> read_file(const char *filename)
{
	vector<Measure> res;
	ifstream in(filename);

	if (!in.good())
	{
		cerr << "Could not open file '" << filename << "'" << endl;
		throw;
	}

	Measure m;
	while (in >> m)
	{
		res.push_back(m);
	}

	return res;
}

int main(int argc, char **argv)
{
	if (argc < 2)
	{
		cerr << "Not enoug arguments. Usage: ./plot <lin_reg_data>.csv" << endl;
		return -1;
	}

	char *filename = argv[1];
	vector<Measure> data = read_file(filename);
	double test_slope = (data[data.size() - 1].y - data[0].y) / (data[data.size() - 1].x - data[0].x);
	for (auto &m : data)
	{
		m.ey = sqrt(pow(m.ey, 2) + pow(m.ex * test_slope, 2));
	}

	TApplication app("linear regression", 0, 0);
	TGraphErrors graph;

	for (int i = 0; i < data.size(); i++)
	{
		graph.SetPoint(i, data[i].x, data[i].y);
		graph.SetPointError(i, data[i].ex, data[i].ey);
	}

	TF1 line_with_intercept("lin_reg", "[0] * x + [1]", data[0].x, data[data.size() - 1].x);
	TF1 line_without_intercept("lin_reg", "[0] * x", data[0].x, data[data.size() - 1].x);
	graph.Fit(&line_with_intercept);
	graph.Fit(&line_without_intercept);

	TCanvas canvas("linear regression");
	canvas.SetGrid();

	graph.Draw("AP");
	graph.SetTitle("e/m linear regression");
	graph.GetXaxis()->SetTitle("2#Delta V [V]");
	graph.GetXaxis()->CenterTitle(true);
	graph.GetYaxis()->SetTitle("B^{2}R^{2} [T^{2}m^{2}]");
	graph.GetYaxis()->CenterTitle(true);

	TLegend legend(0.15, 0.7, 0.3, 0.85);
	legend.AddEntry(&graph, "dati", "LE");
	legend.AddEntry(&line_with_intercept, "fit with q", "L");
	legend.AddEntry(&line_without_intercept, "fit without q", "L");
	legend.Draw();

	double moe_q = line_with_intercept.GetParameter(0);
	double moe_q_err = line_with_intercept.GetParError(0);
	double moe_noq = line_without_intercept.GetParameter(0);
	double moe_noq_err = line_without_intercept.GetParError(0);

	cout << "Results with intercept:" << endl
		 << "e/m = " << 1 / moe_q << " +- " << (1 / pow(moe_q, 2)) * moe_q_err << " C/Kg" << endl
		 << "Results without intercept:" << endl
		 << "e/m = " << 1 / moe_noq << " +- " << (1 / pow(moe_noq, 2)) * moe_noq_err << " C/Kg" << endl;

	app.Run();
}
