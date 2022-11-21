# Introduction

The aim for this project is to analyze the countries using the socio-economic and health factors that describes the overall development of nations. It is a very simple and short project to work with PCA and K-means.

## The data

Data come frome [Kaggle](https://www.kaggle.com/datasets/vipulgohel/clustering-pca-assignment), it contains :
- Name of the country,
- Death of children under five years of age per 1000 live births,
- Exports of goods and services; Exports of goods and services given as %age of the Total GDP;
- Imports of goods and services, Given as %age of the Total GDP;
- Net income per person;
- The measurement of the annual growth rate of the Total GDP;
- The average number of years a newborn child would live if the current mortality patterns are to remain the same;
- The number of children born to each woman if the current age-fertility rates remain the same.

## The results

The biplot of the ACP shows that dimension 2 gives information about the economic development of countries and dimension 1 about import-export. We can see that fertility, child mortality and life expectency are highly corraleted to the economic development of the country.

![biplot](https://user-images.githubusercontent.com/45072645/203086510-165e2539-aee3-4235-b1e9-19ad0b9e8034.png)

The individuals pca plot bellow shows that according to our previous analysis, the countries where the socio-economic state is the most critical are Nigeria and Haiti.

![pca individuals](https://user-images.githubusercontent.com/45072645/203086855-ab0ed9d6-470a-4630-a75a-61fc5032b4c4.png)

Finally, the clustering plot bellow obtained through k-means algorithm shows that among the countries with the most critical socio-economic situation are coutries like Chad, Lesotho, Liberia.

![clusters](https://user-images.githubusercontent.com/45072645/203088419-8c4d4ac2-3570-40e9-b166-e017b61373a0.png)
