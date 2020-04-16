// same model as v1, but estimates the DO threshold directly

data {
 // fullness data
 // terms that are the same for diets and fullness
  int N; //the number of fullness observations
  int Kx; //the number of columns in the model matrix
  int Kz; // the number of columns in random effect matrix
  int do_col; // which column contains DO, if any. 

  real y[N]; //the response
  matrix[N,Kx] X; //the model matrix 
  matrix[N,Kz] Z; // random effects
  real seobs[N];  // the SE  observations
  real usd_prior; // the scale parameter for random effect parameters


}

parameters {
//Fullness parameters
  vector[Kx] beta; //the regression parameters
  vector[Kz] uraw; // the random effect parameters
  real ubar; // random effects mean
  real <lower =0, upper = 5> usd; // random effects sd
  real <lower =0> sigma; //the standard deviation
  real <lower = 2.0, upper = 8 > do_thresh; // the DO threshold

  
}
transformed parameters {

  vector[N] linpredX;
  vector[N] linpredZ;
  vector[Kz] u;
  vector[N] sigmatotal;
  matrix[N, Kx] newX;
  vector[N] log_lik;
  vector[N] yhat_loglik;
  // this is a modified version of the matrix X wherein DO is changed to a piecewise function
  newX = X;
  
  // calculates piecewise covariate for estimating DO effect

if(do_col >0) {
  for (i in 1:N) {
    if (X[i,do_col]>do_thresh) {
      newX[i,do_col]=0;
    } else {
      newX[i,do_col]= do_thresh - X[i,do_col];
  }
  } 
}
  
  // non centered random effects
   u = ubar + uraw*usd; // the random effects

  linpredX = newX*beta;
  linpredZ = Z*u;

  yhat_loglik = linpredX + linpredZ;

  for(i in 1:N) sigmatotal[i]=sqrt(sigma^2 + seobs[i]^2);


for (i in 1:N){
    log_lik[i] = normal_lpdf (y[i]| yhat_loglik[i], sigmatotal[i]);
  }
  
}
model {
vector[N] yhat;

// priors and setup
ubar ~ cauchy(0,2.5);
uraw~normal(0,1);
sigma~ cauchy(0,2.5);
usd ~ cauchy(0,usd_prior);
do_thresh ~ skew_normal(5, 10, -10); // skew normal distribution for do_threshold because we have prior iinformation that it is likely <5 or 6.


for(i in 1:Kx)  beta[i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008

yhat = linpredX + linpredZ;
y ~ normal(yhat,sigmatotal);

}

generated quantities {

  vector[N] y_pred_check;
  real do_effect;
  real effect_X;
  
  if (do_thresh < 2.0 ) {
    effect_X = 0;
    } else {
      effect_X = do_thresh - 2.0;
    }

  
  for(i in 1:N) y_pred_check[i] = normal_rng((linpredX[i]+linpredZ[i]),sigmatotal[i]);
 if (do_col>0){
  do_effect = effect_X  * beta[do_col]; // this is the linear prediction of effect at DO = 2.5 mg
 } else {
   do_effect =  0;
 }


}
