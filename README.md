# Developing Indices of Prey Biomass from Predator Stomach Contents

This repository contains the code used to perform the analyses in our 2021 paper titled ["Predator stomach contents can provide accurate indices of prey biomass."](https://watermark.silverchair.com/fsab026.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA1owggNWBgkqhkiG9w0BBwagggNHMIIDQwIBADCCAzwGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMZ9CIHzZTmFSvWsZuAgEQgIIDDQ6q_Sbj3E9r-cmpIyiUwoAiX3-DW5qFBzMWVSUs7cUj5F2wME03qIDfuEgd8TKAFObWcd3AFTplaytUUVrvtGgmXcXDacXj9Ud3-0V91XA0ot7w5CfqbO4BOYEcXeW6NHRTC3vuIW1oeFRpTiA_DT5NSnKCLi-dTS8VU-mRJoYXXDrjZhPBiB1rxgrsQ3WL4LS7KIJSJwzQw84Z7OVqIVy3RCxa1hElob-1aPm1Bc0cnyoMVsipXaWPNCOaVgxNczYkRQC7T4lIxVc4pt-51E9PradH4ipddupcBGkPcBSasv9QBZNetG_iwiR2bADlZGOk-0X5a6H1wq-f6GYQm4EeiKSqkhM9toGHKyblipCzLX545EOZXWrMV6H8vgbVXngVgZ1pyyDtTemfsQ8LcUsvnex38h8FPRxLLIs-qF784_GJUFyocdRFqOmT8RSG7kmFzQYTu54Iama8agkOf79xjrIW2dcTY7vOBH4BO6i8T35rH9xTsPUy1l-H5nl2p8glXT9gGP5FJpP6ckumE9Np0mWXqe9lRw9FE-dQqLc3hpRq_VLnLJcj0VKVfaaLUDFXlqpASWl4rkyFkS7KEbbia2J-n3OmVJ7C8fc_WSwsbEXmhW8mEHaKb3y7WMZfWbU505LL6a5rL6UO--hdmnkq6vujowRR3QbzxMQJroV5rBk_scq_cCLIrqgW4DsgWVLf0gmGbBImXWSp-QRYrl1ILWsEwtYgdzgyDIPx6xj0Bt2MhYHEoZ-u035-WFHualRQcna4zab-E2i5Vr8By-RPY9_I787Kv5HOl1gZE3r27UhDSipdOBq07IOMvt9aMLa_tWmVjCjppNkFdrrYVhYKxLUdDVozaNNJApZ3yYi-_PNNanTSCClCp77xZLDdVqINCfSEH03JUxP0WHZOQw_1nYkxlQXnPu6guPcW6aQuJF2H2XFr3lFn040IDA0E0X9Y_apSHJJTQRp8VyIDkCBa9HhVLRae6INTe6LZ9_cysq-xdcBzUQgWS08AgPqr-bbUqnVWakNS5r_eUFo)

## Citation
Elizabeth L Ng, Jonathan J Deroba, Timothy E Essington, Arnaud Grüss, Brian E Smith, James T Thorson, Predator stomach contents can provide accurate indices of prey biomass, ICES Journal of Marine Science, Volume 78, Issue 3, July 2021, Pages 1146–1159, https://doi.org/10.1093/icesjms/fsab026

## Abstract
Diet-based annual biomass indices can potentially use predator stomach contents to provide information about prey biomass and may be particularly useful for species that are otherwise poorly sampled, including ecologically important forage fishes. However, diet-based biomass indices may be sensitive to underlying ecological dynamics between predators and prey, such as predator functional responses and changes in overlap in space and time. To evaluate these factors, we fit spatio-temporal models to stomach contents of five Atlantic herring (Clupea harengus) predators and survey catch data for predators and Atlantic herring. We identified drivers of variation in stomach contents, evaluated spatial patterns in stomach content data, and produced predator-specific indices of seasonal Atlantic herring biomass. After controlling for spatio-temporal processes and predator length, diet-based indices of biomass shared similar decadal trends but varied substantially between predators and seasons on shorter time scales. Diet-based indices reflected prey biomass more than prey availability, but weak correlations indicated that not all biological processes were controlled for. Results provide potential guidance for developing diet-based biomass indices and contribute to a body of evidence demonstrating the utility of predator diet data to provide information about relative prey biomass.

## Findings

Potential relationships (shading) between a diet-based index of prey biomass and true underlying prey abundance, illustrating accurate reflection of relative abundance (left panel), hyper-stable index of abundance due to Type II functional response or predator tracking of prey (centre panel), or absence of relationship between index and prey abundance (right panel).

![figure 1](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%201%20conceptual%20diagram.png)


Annually averaged biomass $\hat{b}_{s,t}$ of Atlantic herring in predator stomachs (g/stomach) after controlling for predator length and standardizing (mean zero and standard deviation of one) across predator–season combinations. Warmer colours indicate regions where predators had, on average, higher masses of Atlantic herring per stomach, as predicted by spatio-temporal standardization models. No spatial estimates were generated for white hake in spring due to sparse data.

![figure 2](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%202%20diet-map-quantile.png)


Relationship between predator length (cm) on the log effect of the first linear predictor ($⁠n$⁠, top row) and second linear predictor (⁠$w$⁠, bottom row) of spatio-temporal models of mean Atlantic herring mass in predator stomachs. Shaded regions indicate 95% prediction intervals. Estimates are plotted for fall (blue lines) and spring (red lines) for each predator. Rug plots (black lines, bottom of each panel) depict the number of predators observed of each length.

![figure 3](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%203%20length-effects.png)


Diet-based annual biomass index estimated with spatio-temporal models from Atlantic herring mass in predator stomachs and controlling for predator length. Models were fit to predator diet data separately for each season. Estimated mean values are shown ± one standard error. Grey line indicates estimated Atlantic herring spawning stock biomass from stock assessment, scaled the mean and standard deviation of the diet index in each panel.

![figure 4](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%204%20diet-index-ts_w-SSB.png)


Diet-based biomass index estimated using spatio-temporal models accounting for predator length plotted against Atlantic herring spawning stock biomass estimated from stock assessment. Indices are standardized across years (mean zero and standard deviation one), error bars represent ± 1 C.V. (SE of estimate), and grey lines indicate a fitted linear regression. Pearson correlation’s coefficient is shown in upper left corner of each panel.

![figure 5](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%205%20assessment-diet-comp-1to1.png)


Annually averaged spatially explicit range overlap $\hat{O}_t$ between Atlantic herring and five predators as estimated from spatio-temporal index standardization models fit to bottom trawl survey biomass data for two seasons for all predator–season combinations. Warmer colours indicate regions were where each predator tended to overlap more with Atlantic herring.

![figure 6](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%206%20overlap-map_range-overlap.png)


Annual index of predator–prey overlap $O_t \in [0,1]$ between Atlantic herring and predators as estimated from spatio-temporal models fit to bottom trawl survey biomass data for two seasons (spring, and fall). Predator and prey biomasses were predicted at nearly continuous locations across the sampling region and the proportion of the prey range encompassed by the predator distribution was calculated for each season in each year.

![figure 7](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%207%20overlap-index-ts_range-overlap.png)


Diet-based biomass index estimated using spatio-temporal models accounting for predator length plotted against overlap index estimated using spatio-temporal models. Indices are standardized across years (mean zero and standard deviation one), error bars represent ± 1 C.V. (SE of estimate), and grey lines indicate a fitted linear regression. Pearson correlation coefficient is shown in upper left corner of each panel.

![figure 8](https://github.com/elizabethng/predator-diets/blob/master/figures/Figure%208%20range-overlap-diet-comp-1to1.png)

