ALTER TABLE public.volume_link ADD PRIMARY KEY (volume, url);
ALTER TABLE ONLY public.volume_link DROP CONSTRAINT volume_link_volume_url_key;

ALTER TABLE public.volume_citation ADD PRIMARY KEY (volume);
ALTER TABLE ONLY public.volume_citation DROP CONSTRAINT volume_citation_volume_key;

ALTER TABLE public.asset_revision ADD PRIMARY KEY (orig);
ALTER TABLE ONLY public.asset_revision DROP CONSTRAINT asset_revision_asset_key;