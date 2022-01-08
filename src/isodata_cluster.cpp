#include<vector>
#include<algorithm>
#include<set>
#include<time.h>
#include<cstdlib>
#include<iostream>
#include <iterator>
using namespace std;

class isodata
{
private:
	unsigned int K;
	unsigned int thetaN;
	double theta_c;
	double theta_s;
	unsigned int maxcombine;
	unsigned int maxiteration;

	unsigned int dim;
	double meandis;
	double alpha;
	unsigned int current_iter;
	vector<vector<int>>dataset;
	typedef vector<double> Centroid;

	struct Cluster
	{
		vector<int>clusterID;
		Centroid center;
		double inner_meandis;
		vector<double>sigma;
	};

	vector<Cluster> clus;
private:
	void init();
	void assign();
	void check_thetaN();
	void update_centers();
	void update_center(Cluster &aa);
	void update_sigma(Cluster &aa);
	void calmeandis();
	void choose_nextstep();
	double distance(const Centroid &er, const int k);
	double distance(const Centroid &cen1, const Centroid &cen2);
	void split(const int kk);
	void check_for_split();
	void merge(const int k1, const int k2);
	void check_for_merge();
	void prepare_for_next_itration();
	void show_result();
public:
	isodata()
	{
		time_t t;
		srand(time(&t));
	}
	void generate_data();
	void apply();
	void set_paras();
};



void isodata::generate_data()
{
	int datanums = 100;
	dim = 2;
	for (int i = 0; i < datanums; i++)
	{
		vector<int>data;
		data.resize(dim);
		for (int j = 0; j < dim; j++)
			data[j] = double(rand()) / RAND_MAX * 100;
		dataset.push_back(data);
	}
}

void isodata::set_paras()
{
	K = 5;
	theta_c = 5;
	theta_s = 0.01;
	maxiteration = 10;
	maxcombine = 2;
	thetaN = 5;
	alpha = 0.3;
	current_iter = 0;
}

void isodata::prepare_for_next_itration()
{
	for (int i = 0; i < clus.size(); i++)
		clus[i].clusterID.clear();
}

void isodata::apply()
{
	init();
	while (current_iter < maxiteration)
	{
		current_iter++;
		assign();
		check_thetaN();
		update_centers();
		calmeandis();
		choose_nextstep();
		if (current_iter < maxiteration)
			prepare_for_next_itration();
	}
	show_result();
}

double isodata::distance(const Centroid &cen, const int k)
{
	double dis = 0;
	for (int i = 0; i < dim; i++)
		dis += pow(cen[i] - dataset[k][i], 2);
	return sqrt(dis);
}

double isodata::distance(const Centroid &center1, const Centroid& center2)
{
	double dis = 0;
	for (int i = 0; i < dim; i++)
		dis += pow(center1[i] - center2[i], 2);
	return sqrt(dis);
}

void isodata::init()
{
	clus.resize(K);
	set<int>aa;
	for (int i = 0; i < K; i++)
	{
		clus[i].center.resize(dim);
		int id = double(rand()) / RAND_MAX*dataset.size();
		while (aa.find(id) != aa.end())
		{
			id = double(rand()) / RAND_MAX*dataset.size();
		}
		aa.insert(id);
		for (int j = 0; j < dim; j++)
			clus[i].center[j] = dataset[id][j];
	}
}

void isodata::assign()
{
	for (int i = 0; i < dataset.size(); i++)
	{
		double mindis = 100000000;
		int th = -1;
		for (int j = 0; j < clus.size(); j++)
		{
			double dis = distance(clus[j].center, i);
			if (dis < mindis)
			{
				mindis = dis;
				th = j;
			}
		}
		clus[th].clusterID.push_back(i);
	}
}

void isodata::check_thetaN()
{
	vector<int>toerase;
	for (int i = 0; i < clus.size(); i++)
	{
		if (clus[i].clusterID.size() < thetaN)
		{
			toerase.push_back(i);
			for (int j = 0; j < clus[i].clusterID.size(); j++)
			{
				double mindis = 10000000;
				int th = -1;
				for (int m = 0; m < clus.size(); m++)
				{
					if (m == i)
						continue;
					double dis = distance(clus[m].center,
						clus[i].clusterID[j]);
					if (dis < mindis)
					{
						mindis = dis;
						th = m;
					}
				}
				clus[th].clusterID.push_back(
					clus[i].clusterID[j]);
			}
			clus[i].clusterID.clear();
		}
	}
	for (vector<Cluster>::iterator it = clus.begin(); it != clus.end();)
	{
		if (it->clusterID.empty())
			it = clus.erase(it);
		else
			it++;
	}
}

void isodata::update_center(Cluster &aa)
{
	Centroid temp;
	temp.resize(dim);
	for (int j = 0; j < aa.clusterID.size(); j++)
	{
		for (int m = 0; m < dim; m++)
			temp[m] += dataset[aa.
			clusterID[j]][m];
	}
	for (int m = 0; m < dim; m++)
		temp[m] /= aa.clusterID.size();
	aa.center = temp;

}

void isodata::update_centers()
{
	for (int i = 0; i < clus.size(); i++)
	{
		update_center(clus[i]);
	}
}
void isodata::update_sigma(Cluster&bb)
{
	bb.sigma.clear();
	bb.sigma.resize(dim);
	for (int j = 0; j < bb.clusterID.size(); j++)
		for (int m = 0; m < dim; m++)
			bb.sigma[m] += pow(bb.center[m] -
			dataset[bb.clusterID[j]][m], 2);
	for (int m = 0; m < dim; m++)
		bb.sigma[m] = sqrt(bb.sigma[m] /
		bb.clusterID.size());

}

void isodata::calmeandis()
{
	meandis = 0;
	for (int i = 0; i < clus.size(); i++)
	{
		double dis = 0;
		for (int j = 0; j < clus[i].
			clusterID.size(); j++)
		{
			dis += distance(clus[i].center,
				clus[i].clusterID[j]);
		}
		meandis += dis;
		clus[i].inner_meandis = dis /
			clus[i].clusterID.size();
	}
	meandis /= dataset.size();
}


void isodata::choose_nextstep()
{
	if (current_iter == maxiteration)
	{
		theta_c = 0;
		//goto step 11
		check_for_merge();
	}
	else if (clus.size() < K / 2)
	{
		check_for_split();
	}
	else if (current_iter % 2 == 0 ||
		clus.size() >= 2 * K)
	{
		check_for_merge();
	}
	else
	{
		check_for_split();
	}

}

void isodata::check_for_split()
{
	for (int i = 0; i < clus.size(); i++)
	{
		update_sigma(clus[i]);
	}
	while (true)
	{
		bool flag = false;
		for (int i = 0; i < clus.size(); i++)
		{
			for (int j = 0; j < dim; j++)
			{
				if (clus[i].sigma[j] > theta_s &&
					(clus[i].inner_meandis>meandis&&
					clus[i].clusterID.size()>
					2 * (thetaN + 1) || clus.size() < K / 2))
				{
					flag = true;
					split(i);
				}
			}
		}
		if (!flag)
			break;
		else
			calmeandis();
	}
}

void isodata::split(const int kk)
{
	Cluster newcluster;
	newcluster.center.resize(dim);

	int th = -1;
	double maxval = 0;
	for (int i = 0; i < dim; i++)
	{
		if (clus[kk].sigma[i] > maxval)
		{
			maxval = clus[kk].sigma[i];
			th = i;
		}
	}
	for (int i = 0; i < dim; i++)
	{
		newcluster.center[i] = clus[kk].center[i];
	}
	newcluster.center[th] -= alpha*clus[kk].sigma[th];
	clus[kk].center[th] += alpha*clus[kk].sigma[th];
	for (int i = 0; i < clus[kk].clusterID.size(); i++)
	{
		double d1 = distance(clus[kk].center, clus[kk].clusterID[i]);
		double d2 = distance(newcluster.center, clus[kk].clusterID[i]);
		if (d2 < d1)
			newcluster.clusterID.push_back(clus[kk].clusterID[i]);
	}
	vector<int>cc; cc.reserve(clus[kk].clusterID.size());
	vector<int>aa;
	//insert_iterator<set<int, less<int> > >res_ins(aa, aa.begin()); 

	set_difference(clus[kk].clusterID.begin(), clus[kk].clusterID.end(),
		newcluster.clusterID.begin(), newcluster.clusterID.end(), inserter(aa, aa.begin()));//差集
	clus[kk].clusterID = aa;
	update_center(newcluster);
	update_sigma(newcluster);
	update_center(clus[kk]);
	update_sigma(clus[kk]);
	clus.push_back(newcluster);
}


void isodata::check_for_merge()
{
	vector<pair<pair<int, int>, double>>aa;
	for (int i = 0; i < clus.size(); i++)
	{
		for (int j = i + 1; j < clus.size(); j++)
		{
			double dis = distance(clus[i].center, clus[j].center);
			if (dis < theta_c)
			{
				pair<int, int>bb(i, j);
				aa.push_back(pair<pair<int, int>, double>(bb, dis));
			}
		}
	}
	// 利用函数对象实现升降排序    
	struct CompNameEx
	{
		CompNameEx(bool asce) : asce_(asce)
		{}
		bool operator()(pair<pair<int, int>, double>const& pl, pair<pair<int, int>, double>const& pr)
		{
			return asce_ ? pl.second < pr.second : pr.second < pl.second; // 《Eff STL》条款21: 永远让比较函数对相等的值返回false    
		}
	private:
		bool asce_;
	};
	sort(aa.begin(), aa.end(), CompNameEx(true));
	set<int>bb;
	int combinenus = 0;
	for (int i = 0; i < aa.size(); i++)
	{
		if (bb.find(aa[i].first.first) == bb.end()
			&& bb.find(aa[i].first.second) == bb.end())
		{
			bb.insert(aa[i].first.first);
			bb.insert(aa[i].first.second);
			merge(aa[i].first.first, aa[i].first.second);
			combinenus++;
			if (combinenus >= maxcombine)
				break;
		}
	}
	for (vector<Cluster>::iterator it = clus.begin(); it != clus.end();)
	{
		if (it->clusterID.empty())
		{
			it = clus.erase(it);
		}
		else
			it++;
	}

}

void isodata::merge(const int k1, const int k2)
{
	for (int i = 0; i < dim; i++)
		clus[k1].center[i] = (clus[k1].center[i] * clus[k1].clusterID.size() +
		clus[k2].center[i] * clus[k2].clusterID.size()) /
		double(clus[k1].clusterID.size() + clus[k2].clusterID.size());
	clus[k1].clusterID.insert(clus[k1].clusterID.end(), 
		clus[k2].clusterID.begin(), clus[k2].clusterID.end());
	clus[k2].clusterID.clear();
}

