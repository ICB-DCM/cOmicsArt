## Batch Correction
***  
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/03-pre-processing.html#select-batch-effect-column" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

### Key Considerations:
Batch correction can be performed during pre-processing if the batch information is already included in your sample annotation. When using DESeq processing, the batch-corrected data will be utilized for visualization purposes. However, for significance analysis, the batch will be treated as a factor.

--- 