"use strict";

exports._getFocusedBlock = just => nothing => () => {
  const focusedBlock = window.roamAlphaAPI.ui.getFocusedBlock();
  if (focusedBlock) {
    return just(focusedBlock);
  } else {
    return nothing;
  }
};
