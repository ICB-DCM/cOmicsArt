### Data Upload via Precompiled Data

***
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/01-required-data-input.html#starting-with-an-rds-object" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

With this option, you can upload previously used data. This allows you to upload 
everything at once. Additionally, you can upload results from previous analyses. It is 
also theoretically possible to upload a SummarizedExperiment object from R, that was 
created with a different tool. Make sure that the first assay is the data you want to actually provide - hence if you did your own preprocessing the respective assay would be the first/ default assay. If done preprocessing somewhere else, we would advice to use the preprocessing option `None` in the proceeding.

To generate an uploadable object, just use the `Save file input to upload later` button.
