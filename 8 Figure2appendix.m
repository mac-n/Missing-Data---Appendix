
%x axis
x=1:16;
%locations of error bars
xx=x-0.2275;
data=[0.9135043	0.9228853	0.9290068	0.9210303	0.9393947	0.9044679	0.9229383	0.8992474	0.9337238	0.9219843	0.9332733	0.9285563	0.9379372	0.9241308	0.9252968	0.9205798
];

data1=[0.001	0.012	8.261690141	0.001	8.261690141	16.41334507	16.41334507	0	16.41334507	0.001	8.261690141	8.261690141	52.03100352	52.03100352	52.03100352	52.03100352
];


%cilower= [0.855325000578686 0.861278450823783 0.8733 0.855698038382231 0.8755 0.818229374645805 0.868401136979585   0.872540455281659       0.876217769371621       0.852151105449759       0.8739       0.8819       0.871284826671955       0.8683508752666       0.871648821063759       0.85581034832091]

%ciupper=[0.888756478553516   0.896839040844578   0.9044   0.87958790851789   0.9033   0.860753530123801   0.902075776751317   0.8994658996   0.907442034253955   0.884262629208305   0.9086   0.9092   0.904850778666379   0.898909999620421   0.905847259033596   0.893899826945089]
data2=[1.676901408	1.069647887	1.432640845	1.574577465	1.910475352	0.036778169	8.150897887	0.005528169	8.573485915	0.57443662	0.570264085	0.628116197	1.812447183	1.646302817	0.80971831	0.618433099
 ];

%data2=data2/1185;
yyaxis left;


 left_color = [0 0 0];
right_color = [0 0 0];

set(0,'defaultAxesColorOrder',[left_color; right_color;left_color]);

%to have two y axes with different scales you need to use dummy
%columns which space the bars out. 
dummydata=[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ];
totaldata=[data(:), dummydata(:), dummydata(:)];


timedata=[dummydata(:),data1(:),data2(:)];

h=bar(x,totaldata,'FaceColor',[0.9,0.9,0.9],'LineWidth',0.5);

% hold on;
%  er=errorbar(xx,data,data-cilower,data-ciupper,'LineWidth',1);
% er.Color = [0 0 0];                            
% er.LineStyle = 'none';
% er.Bar.LineStyle = 'dotted';
% %er.CapSize = 5;
% hold off;
ylabel('Two Class AUC');
set(gca,"YLim",[0.88 0.95]);
set(gca,"XLim",[0 17]);
% yl=yline(0.91,'--','Complete Data AUC RF/SVM');
% yl.LabelHorizontalAlignment='left';
%  yl.FontSize=14
%  yl2=yline(0.89,'--','NB');
% yl2.LabelHorizontalAlignment='left';
%  yl2.FontSize=14
%set(gca,'LineWidth',2,'TickLength',[0.02,0.01],'TickDir','out');
set(gca,'FontSize',16)
set(gca,'TickLength',[0,0])
%set(gca,"ylabel","Accuracy R^{2}");
%colororder({'k','k','k'});
%set(fig,'defaultAxesColorOrder',[0 0 0; 0 0 0]);
title('30% Missing Values')

xtickangle(0);
ycolor="black";

%bar(totaldata);
%bar(alldata);
yyaxis right;

hb=bar(x,timedata);
xticks(x);
xticklabels({"A" "B" "C" "D" "E" "F*"  "G*"  "H" "I" "J" "K" "L" "M*" "N" "O*" "P"});
box off;

hb(2).FaceColor = [0.5 0.5 0.5];


hb(3).FaceColor = [0.1 0.1 0.1];
hold off
set(gca, 'YScale', 'log');
%set(gca,"YLim",[0 3]);
%set(gca,'LineWidth',3,'TickLength',[0.00,0.00],'TickDir','out');
ycolor="black";
set(gca,'FontSize',16);
ylabel("Computation time, log");


leg=legend([h(1),hb(2),hb(3)],{'Two Class AUC','Imputation Time','Classification Time'},'NumColumns', 1);
leg.Location='southeast'
      
     
