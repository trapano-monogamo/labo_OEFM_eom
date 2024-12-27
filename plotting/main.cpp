#include <iostream>
#include <fstream>
#include <vector>

#include "TApplication.h"
#include "TCanvas.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TLegend.h"
#include "TAxis.h"
#include "TMath.h"

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
	if (argc < 3)
	{
		cerr << "Not enoug arguments. Usage: ./plot <lin_reg_data>.csv <graph title>" << endl;
		return -1;
	}

	cout << argv[2] << endl;

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
	graph.Fit(&line_without_intercept);
	graph.Fit(&line_with_intercept /*, "+"*/);

	TCanvas canvas("linear regression","linear regression", 1500,1200);
	canvas.SetGrid();

	graph.SetMarkerStyle(20);
	graph.SetMarkerSize(1.5);

	graph.Draw("APsame");
	graph.SetTitle(argv[2]);
	graph.GetXaxis()->SetTitle("2#DeltaV [V]");
	graph.GetXaxis()->CenterTitle(true);
	graph.GetYaxis()->SetTitle("B^{2}R^{2} [T^{2}m^{2}]");
	graph.GetYaxis()->CenterTitle(true);

	TLegend legend(0.15, 0.7, 0.3, 0.85);
	legend.AddEntry(&graph, "data", "LE");
	legend.AddEntry(&line_with_intercept, "fit: mx+q", "L");
	// legend.AddEntry(&line_without_intercept, "fit: mx", "L");
	legend.Draw();

	double expValue = 1.758820e11;

	double moe_q = line_with_intercept.GetParameter(0);
	double moe_q_err = line_with_intercept.GetParError(0);
	double moe_noq = line_without_intercept.GetParameter(0);
	double moe_noq_err = line_without_intercept.GetParError(0);

	double eom_q = 1 / moe_q;
	double eom_q_err = (1 / pow(moe_q, 2)) * moe_q_err;
	double t_q = abs(eom_q - expValue) / eom_q_err;

	double eom_noq = 1 / moe_noq;
	double eom_noq_err = (1 / pow(moe_noq, 2)) * moe_noq_err;
	double t_noq = abs(eom_noq - expValue) / eom_noq_err;

	cout << endl
		 << "Results with intercept:" << endl
		 << "e/m = " << eom_q << " +- " << eom_q_err << " C/Kg" << endl
		 << "p(t)    = " << 2 * TMath::StudentI(-t_q, graph.GetN() - 2) << endl
		 << "p(chi2) = " << line_with_intercept.GetProb() << endl << endl
		 << "Results without intercept:" << endl
		 << "e/m = " << eom_noq << " +- " << eom_noq_err << " C/Kg" << endl
		 << "p(t)    = " << 2 * TMath::StudentI(-t_noq, graph.GetN() - 2) << endl
		 << "p(chi2) = " << line_without_intercept.GetProb() << endl;

	app.Run();
}
