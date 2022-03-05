"use strict";

exports._getFocusedBlockMetadata = just => nothing => () => {
  const focusedBlockMetadata = window.roamAlphaAPI.ui.getFocusedBlock();
  if (focusedBlockMetadata) {
    return just(focusedBlockMetadata);
  } else {
    return nothing;
  }
};
