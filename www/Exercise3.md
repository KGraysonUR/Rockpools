<p></p>
## Exercise 3 - Data manipulation 
This dataset contains morphological, island, and taxonomic information about a lot of birds. Sometimes the data is not in the proper format to directly answer a question. For example, **how many distinct species are on each island?** This is shown in the following pivot table.

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;background-color: #EEE;"> Island ID </th>
   <th style="text-align:left;background-color: #EEE;"> Species L69 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;background-color: white;"> Balt_SS </td>
   <td style="text-align:left;background-color: white;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Cldw </td>
   <td style="text-align:left;background-color: white;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Cmp </td>
   <td style="text-align:left;background-color: white;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Cocos </td>
   <td style="text-align:left;background-color: white;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Cwly </td>
   <td style="text-align:left;background-color: white;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> DMaj </td>
   <td style="text-align:left;background-color: white;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Drwn_Clp </td>
   <td style="text-align:left;background-color: white;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Endrb </td>
   <td style="text-align:left;background-color: white;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Esp_Hd </td>
   <td style="text-align:left;background-color: white;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Flor_Chrl </td>
   <td style="text-align:left;background-color: white;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Frn_Nrb </td>
   <td style="text-align:left;background-color: white;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Gnov_Twr </td>
   <td style="text-align:left;background-color: white;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Grd_EsHd </td>
   <td style="text-align:left;background-color: white;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Grd_FlCh </td>
   <td style="text-align:left;background-color: white;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Isa_Alb </td>
   <td style="text-align:left;background-color: white;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Isa_Alb,Frn_Nrbr </td>
   <td style="text-align:left;background-color: white;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> LHrm_Crsm </td>
   <td style="text-align:left;background-color: white;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Mrch_Bndl </td>
   <td style="text-align:left;background-color: white;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Pnt_Abng </td>
   <td style="text-align:left;background-color: white;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Pnz_Dnc </td>
   <td style="text-align:left;background-color: white;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Rab_Jrv </td>
   <td style="text-align:left;background-color: white;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> SCris_Chat </td>
   <td style="text-align:left;background-color: white;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> SCru_Inde </td>
   <td style="text-align:left;background-color: white;"> 11 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> SFe_Brngt </td>
   <td style="text-align:left;background-color: white;"> 7 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Snti_Jams </td>
   <td style="text-align:left;background-color: white;"> 10 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Trtu_Brat </td>
   <td style="text-align:left;background-color: white;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Unk </td>
   <td style="text-align:left;background-color: white;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;background-color: white;"> Wlf_Wnm </td>
   <td style="text-align:left;background-color: white;"> 6 </td>
  </tr>
</tbody>
</table>

Now use the "Pivot", "Explore", or "Transform" tabs of the Data area to try to recreate these results. Once this is done, ask a different question that requires some data processing and include your results below.
