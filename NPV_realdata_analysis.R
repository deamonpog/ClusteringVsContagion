###############
#analysis for the real data NPV paper
#Xiaoxia Champon
#Nov 26, 2023
##############
library(mgcv)
library(mgcViz)


cc_diffusion_real=read.csv("RealDFSData_CCDiffusion_OrientedParams01.csv")
cc_diffusion_real_RNN=read.csv("RealRNNData_CCDiffusion_OrientedParams01.csv")

cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "facebook_combined_edges"] <- 'FBC'
cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "HR_edges"] <- 'HR'
cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "HU_edges"] <- 'HU'
cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "large_twitch_edges"] <- 'LT'
cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "musae_facebook_edges"] <- 'MFB'
cc_diffusion_real$NetSource[cc_diffusion_real$NetSource == "RO_edges"] <- 'RO'

cc_diffusion_real_RNN$NetSource[cc_diffusion_real_RNN$NetSource == "HR_edges"] <- 'HR'
cc_diffusion_real_RNN$NetSource[cc_diffusion_real_RNN$NetSource == "HU_edges"] <- 'HU'
cc_diffusion_real_RNN$NetSource[cc_diffusion_real_RNN$NetSource == "large_twitch_edges"] <- 'LT'
cc_diffusion_real_RNN$NetSource[cc_diffusion_real_RNN$NetSource == "musae_facebook_edges"] <- 'MFB'
cc_diffusion_real_RNN$NetSource[cc_diffusion_real_RNN$NetSource == "RO_edges"] <- 'RO'

cc_diffusion_real_sub=cc_diffusion_real[cc_diffusion_real$NPV!=0,]
cc_diffusion_real_sub_RNN=cc_diffusion_real_RNN[cc_diffusion_real_RNN$NPV!=0,]

cc_diffusion_real_sub$Model= relevel(as.factor(cc_diffusion_real_sub$Model), ref="LFTM")
cc_diffusion_real_sub$NetSource= relevel(as.factor(cc_diffusion_real_sub$NetSource), ref="RO")

###################
#RF
#base model: just cc
cc_npv=gam(NPV~s(CC_mean),data=cc_diffusion_real_sub)
summary(cc_npv)
cc_npv_model=getViz(cc_npv)
#print(plot(cc_npv_model, allTerms = T), pages = 1)

cc_final=gam(FinalInf~s(CC_mean),data=cc_diffusion_real_sub)
summary(cc_final)
cc_final_model=getViz(cc_final)
#print(plot(cc_final_model, allTerms = T), pages = 1)

#cc by network and model type
cc_npv_modelby=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=cc_diffusion_real_sub)
summary(cc_npv_modelby)
cc_npv_model_byplot=getViz(cc_npv_modelby)
#print(plot(cc_npv_model_byplot, allTerms = T), pages = 1)

cc_final_modelby=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=cc_diffusion_real_sub)
summary(cc_final_modelby)
cc_final_model_byplot=getViz(cc_final_modelby)
#print(plot(cc_final_model_byplot, allTerms = T), pages = 1)


##base model just use model parameters
para_npv=gam(NPV~s(ModelParam01),data=cc_diffusion_real_sub)
summary(para_npv)
para_npv_model=getViz(para_npv)
print(plot(para_npv_model, allTerms = T), pages = 1)

para_final=gam(FinalInf~s(ModelParam01),data=cc_diffusion_real_sub)
summary(para_final)
para_final_model=getViz(para_final)
print(plot(para_final_model, allTerms = T), pages = 1)

#para by network and model type
para_npv_modelby=gam(NPV~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=cc_diffusion_real_sub)
summary(para_npv_modelby)
cc_npv_model_byplot=getViz(para_npv_modelby)
print(plot(para_npv_model_byplot, allTerms = T), pages = 1)

para_final_modelby=gam(FinalInf~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=cc_diffusion_real_sub)
summary(para_final_modelby)
para_final_model_byplot=getViz(para_final_modelby)
print(plot(para_final_model_byplot, allTerms = T), pages = 1)


##full model including interactions
cc_npv_fll=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+
                   s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=cc_diffusion_real_sub)
summary(cc_npv_modelby)
cc_npv_model_byplot=getViz(cc_npv_modelby)
print(plot(cc_npv_model_byplot, allTerms = T), pages = 1)

final_model_full=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+
                         s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=cc_diffusion_real_sub)
summary(final_model_full)
final_model_full_plot=getViz(final_model_full)
print(plot(final_model_full_plot, allTerms = T), pages = 1)


#RNN
#base model: just cc
cc_npv_RNN=gam(NPV~s(CC_mean),data=cc_diffusion_real_sub_RNN)
summary(cc_npv_RNN)
cc_npv_model_RNN=getViz(cc_npv_RNN)
print(plot(cc_npv_model_RNN, allTerms = T), pages = 1)

cc_final_RNN=gam(FinalInf~s(CC_mean),data=cc_diffusion_real_sub_RNN)
summary(cc_final_RNN)
cc_final_model_RNN=getViz(cc_final_RNN)
print(plot(cc_final_model_RNN, allTerms = T), pages = 1)

#cc by network and model type
cc_npv_modelby_RNN=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=cc_diffusion_real_sub_RNN)
summary(cc_npv_modelby_RNN)
cc_npv_model_byplot_RNN=getViz(cc_npv_modelby_RNN)
print(plot(cc_npv_model_byplot_RNN, allTerms = T), pages = 1)

cc_final_modelby_RNN=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=cc_diffusion_real_sub_RNN)
summary(cc_final_modelby_RNN)
cc_final_model_byplot_RNN=getViz(cc_final_modelby_RNN)
print(plot(cc_final_model_byplot_RNN, allTerms = T), pages = 1)


##base model just use model parameters
para_npv_RNN=gam(NPV~s(ModelParam01),data=cc_diffusion_real_sub_RNN)
summary(para_npv_RNN)
para_npv_model_RNN=getViz(para_npv_RNN)
print(plot(para_npv_model_RNN, allTerms = T), pages = 1)

para_final_RNN=gam(FinalInf~s(ModelParam01),data=cc_diffusion_real_sub_RNN)
summary(para_final_RNN)
para_final_model_RNN=getViz(para_final_RNN)
print(plot(para_final_model_RNN, allTerms = T), pages = 1)

#para by network and model type
para_npv_modelby_RNN=gam(NPV~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=cc_diffusion_real_sub_RNN)
summary(para_npv_modelby_RNN)
cc_npv_model_byplot_RNN=getViz(para_npv_modelby_RNN)
print(plot(para_npv_model_byplot_RNN, allTerms = T), pages = 1)

para_final_modelby_RNN=gam(FinalInf~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=cc_diffusion_real_sub_RNN)
summary(para_final_modelby_RNN)
para_final_model_byplot_RNN=getViz(para_final_modelby_RNN)
print(plot(para_final_model_byplot_RNN, allTerms = T), pages = 1)


##full model including interactions
cc_npv_fll_RNN=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+
                       s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=cc_diffusion_real_sub_RNN)
summary(cc_npv_modelby_RNN)
cc_npv_model_byplot_RNN=getViz(cc_npv_modelby_RNN)
print(plot(cc_npv_model_byplot_RNN, allTerms = T), pages = 1)

final_model_full_RNN=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=cc_diffusion_real_sub_RNN)
summary(final_model_full_RNN)
final_model_full_plot_RNN=getViz(final_model_full_RNN)
print(plot(final_model_full_plot_RNN, allTerms = T), pages = 1)


##################


###function to run the analysis
model_build_step <- function (real_data_set) {
    #base model: just cc
    cc_npv=gam(NPV~s(CC_mean),data=real_data_set)
    summary(cc_npv)
    cc_npv_model=getViz(cc_npv)
    print(plot(cc_npv_model, allTerms = T), pages = 1)
    
    cc_final=gam(FinalInf~s(CC_mean),data=real_data_set)
    summary(cc_final)
    cc_final_model=getViz(cc_final)
    print(plot(cc_final_model, allTerms = T), pages = 1)
    
    #cc by network and model type
    cc_npv_modelby=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=real_data_set)
    summary(cc_npv_modelby)
    cc_npv_model_byplot=getViz(cc_npv_modelby)
    print(plot(cc_npv_model_byplot, allTerms = T), pages = 1)
    
    cc_final_modelby=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource),data=real_data_set)
    summary(cc_final_modelby)
    cc_final_model_byplot=getViz(cc_final_modelby)
    print(plot(cc_final_model_byplot, allTerms = T), pages = 1)
    
    
    ##base model just use model parameters
    para_npv=gam(NPV~s(ModelParam01),data=real_data_set)
    summary(para_npv)
    para_npv_model=getViz(para_npv)
    print(plot(para_npv_model, allTerms = T), pages = 1)
    
    para_final=gam(FinalInf~s(ModelParam01),data=real_data_set)
    summary(para_final)
    para_final_model=getViz(para_final)
    print(plot(para_final_model, allTerms = T), pages = 1)
    
    #para by network and model type
    para_npv_modelby=gam(NPV~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=real_data_set)
    summary(para_npv_modelby)
    cc_npv_model_byplot=getViz(para_npv_modelby)
    print(plot(para_npv_model_byplot, allTerms = T), pages = 1)
    
    para_final_modelby=gam(FinalInf~s(ModelParam01)+Model+NetSource+s(ModelParam01,by=Model),data=real_data_set)
    summary(para_final_modelby)
    para_final_model_byplot=getViz(para_final_modelby)
    print(plot(para_final_model_byplot, allTerms = T), pages = 1)
    
    
    ##full model including interactions
    cc_npv_fll=gam(NPV~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+
                       s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=real_data_set)
    summary(cc_npv_modelby)
    cc_npv_model_byplot=getViz(cc_npv_modelby)
    print(plot(cc_npv_model_byplot, allTerms = T), pages = 1)
    
    final_model_full=gam(FinalInf~s(CC_mean)+Model+NetSource+s(CC_mean,by=Model)+s(CC_mean,by=NetSource)+
                             s(ModelParam01)+s(ModelParam01,by=Model)+s(ModelParam01,CC_mean),data=real_data_set)
    summary(final_model_full)
    final_model_full_plot=getViz(final_model_full)
    print(plot(final_model_full_plot, allTerms = T), pages = 1)
    
}

# model_build_step (cc_diffusion_real_sub)
# model_build_step (cc_diffusion_real_sub_RNN)