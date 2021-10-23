final: prev: {
  fennel = prev.fennel.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "bakpakin";
      repo = "Fennel";
      rev = "6ba3c845c98d7371c5bb6869a08f4698960573fe";
      sha256 = "sha256-97NfFaq4Xz1mwzcAbYDT98Eyd1RJRd41gIj+64x3teE=";
    };
  });
}
